open Core.Std
open Async.Std
open Jenga_lib.Api
let mapD = Dep.map
let bindD = Dep.bind
let rel = Path.relative
let ts = Path.to_string
let root = Path.the_root
let bash command =
  Action.process ~dir:root ~prog:"bash" ~args:["-c"; command] ()
let bashf fmt = ksprintf bash fmt
let nonBlank s = match String.strip s with | "" -> false | _ -> true
let relD ~dir  str = Dep.path (rel ~dir str)
let chopSuffixExn str = String.slice str 0 (String.rindex_exn str '.')
let fileNameNoExtNoDir path = (Path.basename path) |> chopSuffixExn
let isInterface path =
  let base = Path.basename path in
  (String.is_suffix base ~suffix:".rei") ||
    (String.is_suffix base ~suffix:".mli")
type moduleName =
  | Mod of string
type libName =
  | Lib of string
let topLibName = ((Lib ("top")))
let libraryFileName = "lib.cma"
let nodeModulesRoot = rel ~dir:root "node_modules"
let buildDirRoot = rel ~dir:root "_build"
let topSrcDir = rel ~dir:root "src"
let tsm ((Mod (s))) = s
let tsl ((Lib (s))) = s
let binaryOutput =
  rel ~dir:(rel ~dir:buildDirRoot (tsl topLibName)) "app.out"
let jsOutput = rel ~dir:(rel ~dir:buildDirRoot (tsl topLibName)) "app.js"
let kebabToCamel =
  String.foldi ~init:""
    ~f:(fun _  ->
          fun accum  ->
            fun char  ->
              if accum = ""
              then Char.to_string char
              else
                if (accum.[(String.length accum) - 1]) = '-'
                then
                  (String.slice accum 0 (-1)) ^
                    ((Char.to_string char) |> String.capitalize)
                else accum ^ (Char.to_string char))
let libToModule ((Lib (name))) =
  ((Mod (((String.capitalize name) |> kebabToCamel))))
let pathToModule path =
  ((Mod (((fileNameNoExtNoDir path) |> String.capitalize))))
let namespacedName ~libName  ~path  =
  (tsm (libToModule libName)) ^ ("__" ^ (tsm (pathToModule path)))
let ocamlDep ~sourcePath  =
  let flag =
    match isInterface sourcePath with | true  -> "-intf" | false  -> "-impl" in
  let action =
    Dep.action_stdout
      (mapD (Dep.path sourcePath)
         (fun ()  ->
            bashf
              "ocamldep -pp refmt -ml-synonym .re -mli-synonym .rei -modules -one-line %s %s"
              flag (ts sourcePath))) in
  let processRawString string =
    match (String.strip string) |> (String.split ~on:':') with
    | original::deps::[] ->
        ((rel ~dir:root original),
          (((String.split deps ~on:' ') |> (List.filter ~f:nonBlank)) |>
             (List.map ~f:(fun m  -> ((Mod (m)))))))
    | _ -> failwith "expected exactly one ':' in ocamldep output line" in
  mapD action processRawString
let ocamlDepCurrentSources ~sourcePath  =
  let srcDir = Path.dirname sourcePath in
  bindD (ocamlDep ~sourcePath)
    (fun (original,deps)  ->
       mapD (Dep.glob_listing (Glob.create ~dir:srcDir "*.{re,rei,ml,mli}"))
         (fun sourcePaths  ->
            let originalModule = pathToModule original in
            let sourceModules =
              (List.map sourcePaths ~f:pathToModule) |> List.dedup in
            (List.filter deps ~f:(fun m  -> m <> originalModule)) |>
              (List.filter
                 ~f:(fun m  ->
                       List.exists sourceModules ~f:(fun m'  -> m = m')))))
let getThirdPartyNpmLibs ~libDir  =
  let packageJsonPath = rel ~dir:libDir "package.json" in
  mapD
    (Dep.action_stdout
       (mapD (Dep.path packageJsonPath)
          (fun ()  ->
             bashf
               "ocamlrun ./node_modules/jengaboot/buildUtils/extractDeps.out %s"
               (ts packageJsonPath))))
    (fun content  ->
       ((String.split content ~on:'\n') |> (List.filter ~f:nonBlank)) |>
         (List.map ~f:(fun name  -> ((Lib (name))))))
let getPpxFlag ~libDir  =
  let packageJsonPath = rel ~dir:libDir "package.json" in
  mapD
    (Dep.action_stdout
       (mapD (Dep.path packageJsonPath)
          (fun ()  ->
             bashf
               "ocamlrun ./node_modules/jengaboot/buildUtils/extractPpxFlag.out %s"
               (ts packageJsonPath))))
    (fun content  ->
       match List.hd
               ((String.split content ~on:'\n') |> (List.filter ~f:nonBlank))
       with
       | None  -> None
       | ((Some (content))) ->
           if content = "" then None else Some content)
let getThirdPartyOcamlfindLibs ~libDir  =
  let packageJsonPath = rel ~dir:libDir "package.json" in
  mapD
    (Dep.action_stdout
       (mapD (Dep.path packageJsonPath)
          (fun ()  ->
             bashf
               "ocamlrun ./node_modules/jengaboot/buildUtils/extractOcamlfindDeps.out %s"
               (ts packageJsonPath))))
    (fun content  ->
       ((String.split content ~on:'\n') |> (List.filter ~f:nonBlank)) |>
         (List.map ~f:(fun name  -> ((Lib (name))))))
let topologicalSort graph =
  let graph = { contents = graph } in
  let rec topologicalSort' currNode accum =
    let nodeDeps =
      match List.Assoc.find graph.contents currNode with
      | None  -> []
      | ((Some (nodeDeps'))) -> nodeDeps' in
    List.iter nodeDeps ~f:(fun dep  -> topologicalSort' dep accum);
    if List.for_all accum.contents ~f:(fun n  -> n <> currNode)
    then
      (accum := (currNode :: (accum.contents));
       graph := (List.Assoc.remove graph.contents currNode)) in
  let accum = { contents = [] } in
  while not (List.is_empty graph.contents) do
    topologicalSort' (fst (List.hd_exn graph.contents)) accum done;
  List.rev accum.contents
let sortedTransitiveThirdPartyNpmLibsIncludingSelf's =
  bindD (getThirdPartyNpmLibs ~libDir:root)
    (fun thirdPartyLibs  ->
       let thirdPartyLibDirs =
         List.map thirdPartyLibs
           ~f:(fun name  -> rel ~dir:nodeModulesRoot (tsl name)) in
       let thirdPartiesThirdPartyLibNamesD =
         Dep.all
           (List.map thirdPartyLibDirs
              ~f:(fun libDir  -> getThirdPartyNpmLibs ~libDir)) in
       mapD thirdPartiesThirdPartyLibNamesD
         (fun thirdPartiesThirdPartyLibNames  ->
            (List.zip_exn thirdPartyLibs thirdPartiesThirdPartyLibNames) |>
              topologicalSort))
let transitiveThirdPartyOcamlfindLibsIncludingSelf's =
  bindD
    (Dep.both (getThirdPartyNpmLibs ~libDir:root)
       (getThirdPartyOcamlfindLibs ~libDir:root))
    (fun (thirdPartyNpmLibs,thirdPartyOcamlfindLibs)  ->
       let thirdPartyLibDirs =
         List.map thirdPartyNpmLibs
           ~f:(fun name  -> rel ~dir:nodeModulesRoot (tsl name)) in
       let thirdPartiesThirdPartyOcamlfindLibNamesD =
         Dep.all
           (List.map thirdPartyLibDirs
              ~f:(fun libDir  -> getThirdPartyOcamlfindLibs ~libDir)) in
       mapD thirdPartiesThirdPartyOcamlfindLibNamesD
         (fun thirdPartiesThirdPartyOcamlfindLibNames  ->
            (thirdPartyOcamlfindLibs @
               (List.concat thirdPartiesThirdPartyOcamlfindLibNames))
              |> List.dedup))
let sortPathsTopologically ~paths  =
  let pathsAsModulesOriginalCapitalization =
    List.map paths ~f:(fun path  -> ((pathToModule path), path)) in
  let pathsAsModules = List.map pathsAsModulesOriginalCapitalization ~f:fst in
  let moduleDepsForPathsD =
    Dep.all
      (List.map paths
         ~f:(fun path  -> ocamlDepCurrentSources ~sourcePath:path)) in
  mapD moduleDepsForPathsD
    (fun moduleDepsForPaths  ->
       ((List.zip_exn pathsAsModules moduleDepsForPaths) |> topologicalSort)
         |>
         (List.map
            ~f:(fun m  ->
                  List.Assoc.find_exn pathsAsModulesOriginalCapitalization m)))
let moduleAliasFileScheme ~buildDir  ~sourceNotInterfacePaths  ~libName  =
  let name extension =
    rel ~dir:buildDir ((tsm (libToModule libName)) ^ ("." ^ extension)) in
  let sourcePath = name "ml" in
  let cmo = name "cmo" in
  let cmi = name "cmi" in
  let cmt = name "cmt" in
  let fileContent =
    (List.map sourceNotInterfacePaths
       ~f:(fun path  ->
             Printf.sprintf "module %s = %s\n" (tsm (pathToModule path))
               (namespacedName ~libName ~path)))
      |> (String.concat ~sep:"") in
  let action =
    bashf
      "ocamlc -bin-annot -g -no-alias-deps -w -49 -w -30 -w -40 -c -impl %s -o %s 2>&1| node_modules/.bin/berror; (exit ${PIPESTATUS[0]})"
      (ts sourcePath) (ts cmo) in
  let compileRule =
    Rule.create ~targets:[cmo; cmi; cmt]
      (mapD (Dep.path sourcePath) (fun ()  -> action)) in
  let contentRule =
    Rule.create ~targets:[sourcePath]
      (Dep.return (Action.save fileContent ~target:sourcePath)) in
  Scheme.rules [contentRule; compileRule]
let compileSourcesScheme ~libDir  ~buildDir  ~libName  ~sourcePaths  =
  let moduleAliasDep extension =
    relD ~dir:buildDir ((tsm (libToModule libName)) ^ ("." ^ extension)) in
  let compileEachSourcePath path =
    mapD
      (Dep.both (getThirdPartyNpmLibs ~libDir)
         (Dep.both (ocamlDepCurrentSources ~sourcePath:path)
            (Dep.both (getThirdPartyOcamlfindLibs ~libDir)
               (getPpxFlag ~libDir))))
      (fun
         (thirdPartyNpmLibs,(firstPartyDeps,(thirdPartyOcamlfindLibNames,ppxFlag)))
          ->
         let isInterface' = isInterface path in
         let hasInterface =
           (not isInterface') &&
             (List.exists sourcePaths
                ~f:(fun path'  ->
                      (isInterface path') &&
                        ((fileNameNoExtNoDir path') =
                           (fileNameNoExtNoDir path)))) in
         let firstPartyCmisDeps =
           (List.filter sourcePaths
              ~f:(fun path  ->
                    let pathAsModule = pathToModule path in
                    List.exists firstPartyDeps
                      ~f:(fun m  -> m = pathAsModule)))
             |>
             (List.map
                ~f:(fun path  ->
                      relD ~dir:buildDir
                        ((namespacedName ~libName ~path) ^ ".cmi"))) in
         let firstPartyCmisDeps =
           if (not isInterface') && hasInterface
           then
             (relD ~dir:buildDir ((namespacedName ~libName ~path) ^ ".cmi"))
             :: firstPartyCmisDeps
           else firstPartyCmisDeps in
         let namespacedName' = namespacedName ~libName ~path in
         let thirdPartiesCmisDep =
           Dep.all_unit
             (List.map thirdPartyNpmLibs
                ~f:(fun libName  ->
                      bindD
                        (Dep.glob_listing
                           (Glob.create
                              ~dir:(rel
                                      ~dir:(rel ~dir:nodeModulesRoot
                                              (tsl libName)) "src")
                              "*.{re,ml}"))
                        (fun thirdPartySources  ->
                           Dep.all_unit
                             (List.map thirdPartySources
                                ~f:(fun sourcePath  ->
                                      relD
                                        ~dir:(rel ~dir:buildDirRoot
                                                (tsl libName))
                                        ((namespacedName ~libName
                                            ~path:sourcePath)
                                           ^ ".cmi")))))) in
         let cmi = rel ~dir:buildDir (namespacedName' ^ ".cmi") in
         let cmo = rel ~dir:buildDir (namespacedName' ^ ".cmo") in
         let cmt = rel ~dir:buildDir (namespacedName' ^ ".cmt") in
         let deps =
           Dep.all_unit ((Dep.path path) :: (moduleAliasDep "cmi") ::
             (moduleAliasDep "cmo") :: (moduleAliasDep "cmt") ::
             (moduleAliasDep "ml") :: thirdPartiesCmisDep ::
             firstPartyCmisDeps) in
         let targets =
           if isInterface'
           then [cmi]
           else if hasInterface then [cmo; cmt] else [cmi; cmo; cmt] in
         let maybePpx =
           match ppxFlag with
           | None  -> ""
           | ((Some (flag))) -> "-ppx '" ^ (flag ^ "'") in
         let ocamlfindPackagesStr =
           if thirdPartyOcamlfindLibNames = []
           then ""
           else
             "-package " ^
               ((thirdPartyOcamlfindLibNames |> (List.map ~f:tsl)) |>
                  (String.concat ~sep:",")) in
         let action =
           bashf
             (if isInterface'
              then
                "ocamlfind ocamlc %s -pp refmt -g -w -30 -w -40 %s -open %s -I %s %s -o %s -c -intf %s 2>&1| node_modules/.bin/berror; (exit ${PIPESTATUS[0]})"
              else
                if String.is_suffix (Path.basename path) ~suffix:".re"
                then
                  "ocamlfind ocamlc %s -pp refmt -bin-annot -g -w -30 -w -40 %s -open %s -I %s %s -o %s -c -intf-suffix .rei -impl %s 2>&1| node_modules/.bin/berror; (exit ${PIPESTATUS[0]})"
                else
                  "ocamlfind ocamlc %s -pp refmt -bin-annot -g -w -30 -w -40 %s -open %s -I %s %s -o %s -c -impl %s 2>&1| node_modules/.bin/berror; (exit ${PIPESTATUS[0]})")
             ocamlfindPackagesStr maybePpx (tsm (libToModule libName))
             (ts buildDir)
             ((List.map thirdPartyNpmLibs
                 ~f:(fun ((Lib (name)))  ->
                       "-I " ^ (ts (rel ~dir:buildDirRoot name))))
                |> (String.concat ~sep:" "))
             (ts (rel ~dir:buildDir namespacedName')) (ts path) in
         Rule.create ~targets (mapD deps (fun ()  -> action))) in
  Scheme.rules_dep (Dep.all (List.map sourcePaths ~f:compileEachSourcePath))
let compileCmaScheme ~sortedSourcePaths  ~libName  ~buildDir  =
  let moduleName = tsm (libToModule libName) in
  let cmaPath = rel ~dir:buildDir libraryFileName in
  let moduleAliasCmoPath = rel ~dir:buildDir (moduleName ^ ".cmo") in
  let cmos =
    List.map sortedSourcePaths
      ~f:(fun path  ->
            rel ~dir:buildDir ((namespacedName ~libName ~path) ^ ".cmo")) in
  let cmosString = (List.map cmos ~f:ts) |> (String.concat ~sep:" ") in
  Scheme.rules
    [Rule.simple ~targets:[cmaPath]
       ~deps:(List.map (moduleAliasCmoPath :: cmos) ~f:Dep.path)
       ~action:(bashf
                  "ocamlc -g -open %s -a -o %s %s %s 2>&1| node_modules/.bin/berror; (exit ${PIPESTATUS[0]})"
                  moduleName (ts cmaPath) (ts moduleAliasCmoPath) cmosString)]
let finalOutputsScheme ~sortedSourcePaths  =
  let buildDir = rel ~dir:buildDirRoot (tsl topLibName) in
  let moduleAliasCmoPath =
    rel ~dir:buildDir ((tsm (libToModule topLibName)) ^ ".cmo") in
  let cmos =
    List.map sortedSourcePaths
      ~f:(fun path  ->
            rel ~dir:buildDir
              ((namespacedName ~libName:topLibName ~path) ^ ".cmo")) in
  let cmosString = (List.map cmos ~f:ts) |> (String.concat ~sep:" ") in
  Scheme.dep
    (mapD
       (Dep.both sortedTransitiveThirdPartyNpmLibsIncludingSelf's
          transitiveThirdPartyOcamlfindLibsIncludingSelf's)
       (fun
          (sortedTransitiveThirdPartyNpmLibsIncludingSelf's',transitiveThirdPartyOcamlfindLibsIncludingSelf's')
           ->
          let transitiveCmaPaths =
            List.map sortedTransitiveThirdPartyNpmLibsIncludingSelf's'
              ~f:(fun ((Lib (name)))  ->
                    rel ~dir:(rel ~dir:buildDirRoot name) libraryFileName) in
          let ocamlfindPackagesStr =
            if transitiveThirdPartyOcamlfindLibsIncludingSelf's' = []
            then ""
            else
              "-linkpkg -package " ^
                ((List.map transitiveThirdPartyOcamlfindLibsIncludingSelf's'
                    ~f:tsl)
                   |> (String.concat ~sep:",")) in
          let action =
            bashf
              "ocamlfind ocamlc %s -g -open %s -o %s %s %s %s 2>&1| node_modules/.bin/berror; (exit ${PIPESTATUS[0]})"
              ocamlfindPackagesStr (tsm (libToModule topLibName))
              (ts binaryOutput)
              ((transitiveCmaPaths |> (List.map ~f:ts)) |>
                 (String.concat ~sep:" ")) (ts moduleAliasCmoPath) cmosString in
          Scheme.rules
            [Rule.simple ~targets:[binaryOutput]
               ~deps:(([moduleAliasCmoPath] @ (cmos @ transitiveCmaPaths)) |>
                        (List.map ~f:Dep.path)) ~action;
            Rule.simple ~targets:[jsOutput] ~deps:[Dep.path binaryOutput]
              ~action:(bashf
                         "js_of_ocaml --source-map --no-inline --debug-info --pretty --linkall -o %s %s"
                         (ts jsOutput) (ts binaryOutput))]))
let compileLibScheme ?(isTopLevelLib= true)  ~srcDir  ~libName  ~buildDir  =
  Scheme.dep
    (bindD (Dep.glob_listing (Glob.create ~dir:srcDir "*.{re,rei,ml,mli}"))
       (fun unsortedPaths  ->
          let sourceNotInterfacePaths =
            List.filter unsortedPaths
              ~f:(fun path  -> not (isInterface path)) in
          mapD (sortPathsTopologically ~paths:unsortedPaths)
            (fun sortedPaths  ->
               Scheme.all
                 [moduleAliasFileScheme ~buildDir ~libName
                    ~sourceNotInterfacePaths;
                 compileSourcesScheme ~libDir:(Path.dirname srcDir) ~buildDir
                   ~libName ~sourcePaths:unsortedPaths;
                 (match isTopLevelLib with
                  | true  ->
                      finalOutputsScheme ~sortedSourcePaths:sortedPaths
                  | false  ->
                      compileCmaScheme ~buildDir ~libName
                        ~sortedSourcePaths:sortedPaths)])))
let dotMerlinScheme ~isTopLevelLib  ~libName  ~dir  =
  let dotMerlinPath = rel ~dir ".merlin" in
  let saveMerlinAction thirdPartyOcamlfindLibNames =
    let dotMerlinContent =
      Printf.sprintf
        {|# This file is autogenerated for
# [Merlin](https://github.com/the-lambda-church/merlin), a static analyser for
# OCaml that provides autocompletion, jump-to-location, recoverable syntax
# errors, type errors detection, etc., that your editor can use. To activate it,
# one usually provides a .merlin file at the root of a project, describing where
# the sources and artifacts are. Since we dictated the project structure, we can
# auto generate .merlin files!

# S is the merlin flag for source files
%s

# Include all the third-party sources too. You might notice that we've put a
# .merlin into each node_modules package. This is subtle; in short, it's to make
# jump-to-location work correctly in conjunction with our build & namespacing
# setup, when you jump into a third-party file.
S %s

# B stands for build (artifacts). We generate ours into _build
B %s

# PKG lists packages found through ocamlfind (aka findlib), a tool for finding
# the location of third-party dependencies. For us, most of our third-party deps
# reside in `node_modules/` (made visible to Merlin through the S command
# above); this PKG command is for discovering the opam/ocamlfind packages.
PKG %s

# FLG is the set of flags to pass to Merlin, as if it used ocamlc to compile and
# understand our sources. You don't have to understand what these flags are for
# now; but if you're curious, go check the jengaroot.ml that generated this
# .merlin at https://github.com/chenglou/jengaboot
FLG -w -30 -w -40 -open %s
|}
        (match isTopLevelLib with | true  -> "S src" | false  -> "")
        (Path.reach_from ~dir (rel ~dir:nodeModulesRoot "**/src"))
        (Path.reach_from ~dir (rel ~dir:buildDirRoot "*"))
        ((thirdPartyOcamlfindLibNames |> (List.map ~f:tsl)) |>
           (String.concat ~sep:" ")) (tsm (libToModule libName)) in
    Action.save dotMerlinContent ~target:dotMerlinPath in
  Scheme.rules
    [Rule.create ~targets:[dotMerlinPath]
       (mapD (getThirdPartyOcamlfindLibs ~libDir:dir) saveMerlinAction)]
let scheme ~dir  =
  ignore dir;
  if dir = root
  then
    (let packageJsonPath = rel ~dir:root "package.json" in
     let dotMerlinDefaultScheme =
       Scheme.rules_dep
         (mapD (getThirdPartyNpmLibs ~libDir:root)
            (fun libs  ->
               let thirdPartyRoots =
                 List.map libs
                   ~f:(fun ((Lib (name)))  ->
                         rel ~dir:nodeModulesRoot name) in
               List.map thirdPartyRoots
                 ~f:(fun path  ->
                       Rule.default ~dir:root [relD ~dir:path ".merlin"]))) in
     Scheme.all
       [dotMerlinScheme ~isTopLevelLib:true ~dir:root ~libName:topLibName;
       Scheme.rules
         [Rule.default ~dir
            [Dep.path binaryOutput;
            Dep.path jsOutput;
            relD ~dir:root ".merlin"]];
       Scheme.exclude (fun path  -> path = packageJsonPath)
         dotMerlinDefaultScheme])
  else
    if Path.is_descendant ~dir:buildDirRoot dir
    then
      (let libName = ((Lib ((Path.basename dir)))) in
       let srcDir =
         match libName = topLibName with
         | true  -> topSrcDir
         | false  -> rel ~dir:(rel ~dir:nodeModulesRoot (tsl libName)) "src" in
       compileLibScheme ~srcDir ~isTopLevelLib:(libName = topLibName)
         ~libName ~buildDir:(rel ~dir:buildDirRoot (tsl libName)))
    else
      if (Path.dirname dir) = nodeModulesRoot
      then
        (let libName = ((Lib ((Path.basename dir)))) in
         dotMerlinScheme ~isTopLevelLib:false ~dir ~libName)
      else Scheme.no_rules
let env = Env.create scheme
let setup () = Deferred.return env
