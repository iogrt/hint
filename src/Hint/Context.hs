module Hint.Context (
      isModuleInterpreted,
      loadModules, getLoadedModules, setTopLevelModules,
      setImports, setImportsQ, setImportsF,
      reset,

      PhantomModule(..),
      cleanPhantomModules,
      addPhantomModule,
      getContext,
      setContextModules,

      supportString, supportShow
) where

import Prelude hiding (mod)

import Data.Char
import Data.Either (partitionEithers)
import Data.List

import Control.Arrow ((***))

import Control.Monad (filterM, unless, guard, foldM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch

import Hint.Base
import Hint.Conversions
import qualified Hint.CompatPlatform as Compat

import qualified Hint.GHC as GHC

import System.Random
import System.FilePath
import System.Directory

import Hint.Configuration (setGhcOption)
import System.IO.Temp

type ModuleText = String

-- When creating a phantom module we have a situation similar to that of
-- @Hint.Util.safeBndFor@: we want to avoid picking a module name that is
-- already in-scope. Additionally, since this may be used with sandboxing in
-- mind we want to avoid easy-to-guess names. Thus, we do a trick similar
-- to the one in safeBndFor, but including a random number instead of an
-- additional digit. Finally, to avoid clashes between two processes
-- that are concurrently running with the same random seed (e.g., initialized
-- with the system time with not enough resolution), we also include the process id
newPhantomModule :: MonadInterpreter m => m PhantomModule
newPhantomModule =
    do n <- liftIO $ randomRIO (0, 9) -- smaller numbers so that filename doesn't grow too much
       p <- liftIO Compat.getPID
       (ls,is) <- allModulesInContext
       let nums = concat [show (abs n::Int), show p, filter isDigit $ concat (ls ++ is)]
       let mod_name = 'M':nums
       --
       tmp_dir <- getPhantomDirectory
       --
       return PhantomModule{pmName = mod_name, pmFile = tmp_dir </> mod_name <.> "hs"}

getPhantomDirectory :: MonadInterpreter m => m FilePath
getPhantomDirectory =
    -- When a module is loaded by file name, ghc-8.4.1 loses track of the
    -- file location after the first time it has been loaded, so we create
    -- a directory for the phantom modules and add it to the search path.
    do mfp <- fromState phantomDirectory
       case mfp of
           Just fp -> return fp
           Nothing -> do tmp_dir <- liftIO getTemporaryDirectory
                         fp <- liftIO $ createTempDirectory tmp_dir "hint"
                         onState (\s -> s{ phantomDirectory = Just fp })
                         setGhcOption $ "-i" ++ fp
                         return fp

allModulesInContext :: MonadInterpreter m => m ([ModuleName], [ModuleName])
allModulesInContext = runGhc getContextNames

getContext :: GHC.GhcMonad m => m ([GHC.Module], [GHC.ImportDecl GHC.GhcPs])
getContext = do
    ctx <- GHC.getContext
    foldM f ([], []) ctx
  where
    f :: (GHC.GhcMonad m) =>
         ([GHC.Module], [GHC.ImportDecl GHC.GhcPs]) ->
         GHC.InteractiveImport ->
         m ([GHC.Module], [GHC.ImportDecl GHC.GhcPs])
    f (ns, ds) i = case i of
      (GHC.IIDecl d)     -> return (ns, d : ds)
      (GHC.IIModule m) -> do n <- GHC.findModule m Nothing; return (n : ns, ds)

modToIIMod :: GHC.Module -> GHC.InteractiveImport
modToIIMod = GHC.IIModule . GHC.moduleName

getContextNames :: GHC.GhcMonad m => m([String], [String])
getContextNames = fmap (map name *** map decl) getContext
    where name = GHC.moduleNameString . GHC.moduleName
          decl = GHC.moduleNameString . GHC.unLoc . GHC.ideclName

setContext :: GHC.GhcMonad m => [GHC.Module] -> [GHC.ImportDecl GHC.GhcPs] -> m ()
setContext ms ds =
  let ms' = map modToIIMod ms
      ds' = map GHC.IIDecl ds
      is = ms' ++ ds'
  in GHC.setContext is

-- Explicitly-typed variants of getContext/setContext, for use where we modify
-- or override the context.
setContextModules :: GHC.GhcMonad m => [GHC.Module] -> [GHC.Module] -> m ()
setContextModules as = setContext as . map (GHC.simpleImportDecl . GHC.moduleName)

addPhantomModule :: MonadInterpreter m
                 => (ModuleName -> ModuleText)
                 -> m PhantomModule
addPhantomModule mod_text =
    do pm <- newPhantomModule
       df <- runGhc GHC.getSessionDynFlags
       let t = GHC.fileTarget df (pmFile pm)
       --
       liftIO $ writeFile (pmFile pm) (mod_text $ pmName pm)
       --
       onState (\s -> s{activePhantoms = pm:activePhantoms s})
       mayFail (do -- GHC.load will remove all the modules from
                   -- scope, so first we save the context...
                   (old_top, old_imps) <- runGhc getContext
                   --
                   runGhc $ GHC.addTarget t
                   res <- runGhc $ GHC.load GHC.LoadAllTargets
                   --
                   if isSucceeded res
                     then do runGhc $ setContext old_top old_imps
                             return $ Just ()
                     else return Nothing)
        `catchIE` (\err -> case err of
                             WontCompile _ -> do removePhantomModule pm
                                                 throwM err
                             _             -> throwM err)
       --
       return pm

removePhantomModule :: forall m. MonadInterpreter m => PhantomModule -> m ()
removePhantomModule pm =
    do -- We don't want to actually unload this module, because that
       -- would mean that all the real modules might get reloaded and the
       -- user didn't require that (they may be in a non-compiling state!).
       -- However, this means that we can't actually delete the file, because
       -- it is an active target. Therefore, we simply take it out of scope
       -- and mark it as "delete me when possible" (i.e., next time the
       -- @loadModules@ function is called).
       --
       isLoaded <- moduleIsLoaded $ pmName pm
       safeToRemove <-
           if isLoaded
             then do -- take it out of scope
                     mod <- findModule (pmName pm)
                     (mods, imps) <- runGhc getContext
                     let mods' = filter (mod /=) mods
                     runGhc $ setContext mods' imps
                     --
                     let isNotPhantom :: GHC.Module -> m Bool
                         isNotPhantom mod' = do
                           not <$> isPhantomModule (moduleToString mod')
                     null <$> filterM isNotPhantom mods'
             else return True
       --
       let file_name = pmFile pm
       runGhc $ do df <- GHC.getSessionDynFlags
                   GHC.removeTarget (GHC.targetId $ GHC.fileTarget df file_name)
       --
       onState (\s -> s{activePhantoms = filter (pm /=) $ activePhantoms s})
       --
       if safeToRemove
         then mayFail $ do res <- runGhc $ GHC.load GHC.LoadAllTargets
                           return $ guard (isSucceeded res) >> Just ()
              `finally` do liftIO $ removeFile (pmFile pm)
         else onState (\s -> s{zombiePhantoms = pm:zombiePhantoms s})

-- Returns a tuple with the active and zombie phantom modules respectively
getPhantomModules :: MonadInterpreter m => m ([PhantomModule], [PhantomModule])
getPhantomModules = do active <- fromState activePhantoms
                       zombie <- fromState zombiePhantoms
                       return (active, zombie)

isPhantomModule :: MonadInterpreter m => ModuleName -> m Bool
isPhantomModule mn = do (as,zs) <- getPhantomModules
                        return $ mn `elem` map pmName (as ++ zs)

-- | Tries to load all the requested modules from their source file.
--   Modules my be indicated by their ModuleName (e.g. \"My.Module\") or
--   by the full path to its source file. Note that in order to use code from
--   that module, you also need to call 'setImports' (to use the exported types
--   and definitions) or 'setTopLevelModules' (to also use the private types
--   and definitions).
--
-- The interpreter is 'reset' both before loading the modules and in the event
-- of an error.
--
-- /IMPORTANT/: Like in a ghci session, this will also load (and interpret)
--  any dependency that is not available via an installed package. Make
--  sure that you are not loading any module that is also being used to
--  compile your application.  In particular, you need to avoid modules
--  that define types that will later occur in an expression that you will
--  want to interpret.
--
-- The problem in doing this is that those types will have two incompatible
-- representations at runtime: 1) the one in the compiled code and 2) the
-- one in the interpreted code. When interpreting such an expression (bringing
-- it to program-code) you will likely get a segmentation fault, since the
-- latter representation will be used where the program assumes the former.
--
-- The rule of thumb is: never make the interpreter run on the directory
-- with the source code of your program! If you want your interpreted code to
-- use some type that is defined in your program, then put the defining module
-- on a library and make your program depend on that package.
loadModules :: MonadInterpreter m => [String] -> m ()
loadModules fs = do -- first, unload everything, and do some clean-up
                    reset
                    doLoad fs `catchIE` (\e -> reset >> throwM e)

doLoad :: MonadInterpreter m => [String] -> m ()
doLoad fs = do targets <- mapM (\f->runGhc $ GHC.guessTarget f Nothing) fs
               --
               reinstallSupportModule targets

-- | Returns True if the module was interpreted.
isModuleInterpreted :: MonadInterpreter m => ModuleName -> m Bool
isModuleInterpreted moduleName = do
  mod <- findModule moduleName
  runGhc $ GHC.moduleIsInterpreted mod

-- | Returns the list of modules loaded with 'loadModules'.
getLoadedModules :: MonadInterpreter m => m [ModuleName]
getLoadedModules = do (active_pms, zombie_pms) <- getPhantomModules
                      ms <- map modNameFromSummary <$> getLoadedModSummaries
                      return $ ms \\ map pmName (active_pms ++ zombie_pms)

modNameFromSummary :: GHC.ModSummary -> ModuleName
modNameFromSummary = moduleToString . GHC.ms_mod

getLoadedModSummaries :: MonadInterpreter m => m [GHC.ModSummary]
getLoadedModSummaries = do
    modGraph <- runGhc GHC.getModuleGraph
    let modSummaries = GHC.mgModSummaries modGraph
    filterM (\modl -> runGhc $ GHC.isLoaded $ GHC.ms_mod_name modl) modSummaries

-- | Sets the modules whose context is used during evaluation. All bindings
--   of these modules are in scope, not only those exported.
--
--   Modules must be interpreted to use this function.
setTopLevelModules :: MonadInterpreter m => [ModuleName] -> m ()
setTopLevelModules ms =
    do loaded_mods_ghc <- getLoadedModSummaries
       --
       let not_loaded = ms \\ map modNameFromSummary loaded_mods_ghc
       unless (null not_loaded) $
         throwM $ NotAllowed ("These modules have not been loaded:\n" ++
                              unlines not_loaded)
       --
       active_pms <- fromState activePhantoms
       ms_mods <- mapM findModule (nub $ ms ++ map pmName active_pms)
       --
       let mod_is_interpr modl = runGhc $ GHC.moduleIsInterpreted modl
       not_interpreted <- filterM (fmap not . mod_is_interpr) ms_mods
       unless (null not_interpreted) $
         throwM $ NotAllowed ("These modules are not interpreted:\n" ++
                              unlines (map moduleToString not_interpreted))
       --
       (_, old_imports) <- runGhc getContext
       runGhc $ setContext ms_mods old_imports

-- | Sets the modules whose exports must be in context. These can be modules
-- previously loaded with 'loadModules', or modules from packages which hint is
-- aware of. This includes package databases specified to
-- 'unsafeRunInterpreterWithArgs' by the @-package-db=...@ parameter, and
-- packages specified by a ghc environment file created by @cabal build --write-ghc-environment-files=always@.
--
--   Warning: 'setImports', 'setImportsQ', and 'setImportsF' are mutually exclusive.
--   If you have a list of modules to be used qualified and another list
--   unqualified, then you need to do something like
--
--   >  setImportsQ ((zip unqualified $ repeat Nothing) ++ qualifieds)
setImports :: MonadInterpreter m => [ModuleName] -> m ()
setImports ms = setImportsF $ map (\m -> ModuleImport m NotQualified NoImportList) ms

-- | A variant of 'setImports' where modules them may be qualified. e.g.:
--
--   @setImportsQ [("Prelude", Nothing), ("Data.Map", Just "M")]@.
--
--   Here, "map" will refer to Prelude.map and "M.map" to Data.Map.map.
setImportsQ :: MonadInterpreter m => [(ModuleName, Maybe String)] -> m ()
setImportsQ ms = setImportsF $ map (\(m,q) -> ModuleImport m (maybe NotQualified (QualifiedAs . Just) q) NoImportList) ms

-- | A variant of 'setImportsQ' where modules may have an explicit import list. e.g.:
--
--   @setImportsF [ModuleImport "Prelude" NotQualified NoImportList, ModuleImport "Data.Text" (QualifiedAs $ Just "Text") (HidingList ["pack"])]@

setImportsF :: MonadInterpreter m => [ModuleImport] -> m ()
setImportsF moduleImports = do
       regularMods <- mapM (findModule . modName) regularImports
       mapM_ (findModule . modName) phantomImports -- just to be sure they exist
       --
       old_qual_hack_mod <- fromState importQualHackMod
       maybe (return ()) removePhantomModule old_qual_hack_mod
       --
       maybe_phantom_module <- do
         if null phantomImports
           then return Nothing
           else do
             let moduleContents = map newImportLine phantomImports
             new_phantom_module <- addPhantomModule $ \mod_name
               -> unlines $ ("module " ++ mod_name ++ " where ")
                          : moduleContents
             onState (\s -> s{importQualHackMod = Just new_phantom_module})
             return $ Just new_phantom_module
       --
       phantom_mods <- case maybe_phantom_module of
         Nothing -> do
           pure []
         Just phantom_module-> do
           phantom_mod <- findModule (pmName phantom_module)
           pure [phantom_mod]
       (old_top_level, _) <- runGhc getContext
       let new_top_level = phantom_mods ++ old_top_level
       runGhc $ setContextModules new_top_level regularMods
       --
       onState (\s ->s{qualImports = phantomImports})
  where
    (regularImports, phantomImports) = partitionEithers
                                     $ map (\m -> if isQualified m || hasImportList m
                                                  then Right m  -- phantom
                                                  else Left m)
                                           moduleImports
    isQualified m = modQual m /= NotQualified
    hasImportList m = modImp m /= NoImportList
    newImportLine m = concat ["import ", case modQual m of
                                            NotQualified -> modName m
                                            ImportAs q -> modName m ++ " as " ++ q
                                            QualifiedAs Nothing -> "qualified " ++ modName m
                                            QualifiedAs (Just q) -> "qualified " ++ modName m ++ " as " ++ q
                             ,case modImp m of
                                 NoImportList -> ""
                                 ImportList l -> " (" ++ intercalate "," l ++ ")"
                                 HidingList l -> " hiding (" ++ intercalate "," l ++ ")"
                             ]

-- | 'cleanPhantomModules' works like 'reset', but skips the
--   loading of the support module that installs '_show'. Its purpose
--   is to clean up all temporary files generated for phantom modules.
cleanPhantomModules :: MonadInterpreter m => m ()
cleanPhantomModules =
    do -- Remove all modules from context
       runGhc $ setContext [] []
       --
       -- Unload all previously loaded modules
       runGhc $ GHC.setTargets []
       _ <- runGhc $ GHC.load GHC.LoadAllTargets
       --
       -- At this point, GHCi would call rts_revertCAFs and
       -- reset the buffering of stdin, stdout and stderr.
       -- Should we do any of these?
       --
       -- liftIO $ rts_revertCAFs
       --
       -- We now remove every phantom module and forget about qual imports
       old_active <- fromState activePhantoms
       old_zombie <- fromState zombiePhantoms
       onState (\s -> s{activePhantoms      = [],
                        zombiePhantoms      = [],
                        importQualHackMod = Nothing,
                        qualImports         = []})
       liftIO $ mapM_ (removeFile . pmFile) (old_active ++ old_zombie)

       old_phantomdir <- fromState phantomDirectory
       onState (\s -> s{phantomDirectory    = Nothing})
       liftIO $ do maybe (return ()) removeDirectory old_phantomdir

-- | All imported modules are cleared from the context, and
--   loaded modules are unloaded. It is similar to a @:load@ in
--   GHCi, but observe that not even the Prelude will be in
--   context after a reset.
reset :: MonadInterpreter m => m ()
reset = do -- clean up context
           cleanPhantomModules
           --
           -- Now, install a support module
           installSupportModule []

-- Load a phantom module with all the symbols from the prelude we need
installSupportModule :: MonadInterpreter m => [GHC.Target] -> m ()
installSupportModule ts = do runGhc $ GHC.setTargets ts
                             mod <- addPhantomModule support_module
                             onState (\st -> st{hintSupportModule = mod})
                             mod' <- findModule (pmName mod)
                             runGhc $ setContext [mod'] []
    --
    where support_module m = unlines [
                               "module " ++ m ++ "( ",
                               "    " ++ _String ++ ",",
                               "    " ++ _show   ++ ")",
                               "where",
                               "",
                               "import qualified Prelude as " ++ _P ++ " (String, Show(show))",
                               "",
                               "type " ++ _String ++ " = " ++ _P ++ ".String",
                               "",
                               _show ++ " :: " ++ _P ++ ".Show a => a -> " ++ _P ++ ".String",
                               _show ++ " = " ++ _P ++ ".show"
                             ]
            where _String = altStringName m
                  _show   = altShowName m
                  _P      = altPreludeName m

-- Call it when the support module is an active phantom module but has been
-- unloaded as a side effect by GHC (e.g. by calling GHC.loadTargets)
reinstallSupportModule :: [GHC.Target] -> MonadInterpreter m => m ()
reinstallSupportModule ts = do pm <- fromState hintSupportModule
                               removePhantomModule pm
                               installSupportModule ts

altStringName :: ModuleName -> String
altStringName mod_name = "String_" ++ mod_name

altShowName :: ModuleName -> String
altShowName mod_name = "show_" ++ mod_name

altPreludeName :: ModuleName -> String
altPreludeName mod_name = "Prelude_" ++ mod_name

supportString :: MonadInterpreter m => m String
supportString = do mod_name <- fromState (pmName . hintSupportModule)
                   return $ concat [mod_name, ".", altStringName mod_name]

supportShow :: MonadInterpreter m => m String
supportShow = do mod_name <- fromState (pmName . hintSupportModule)
                 return $ concat [mod_name, ".", altShowName mod_name]

-- SHOULD WE CALL THIS WHEN MODULES ARE LOADED / UNLOADED?
-- foreign import ccall "revertCAFs" rts_revertCAFs :: IO ()
