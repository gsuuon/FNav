open System
open System.IO
open System.Drawing

open Gsuuon.Console.Buffer
open Gsuuon.Console.Vterm
open Gsuuon.Console.Style

let sep = "\n"
let pathSep = string Path.DirectorySeparatorChar

let usage = """Navigate through filesystem and pick a directory to stdout.

Usage:
    > fn {flag}

Flags:
    -m          sort by modified date (defaults to sorting by name)
    -a          sort by accessed date

Example:
    > cd (fn)
"""

let formatPathBreadcrumbs (path: string) =
    let home = Environment.SpecialFolder.UserProfile |> Environment.GetFolderPath

    path.Replace(home, "~").Replace(pathSep, " > ")

let addDir path parent = Path.Combine [| parent; path |]

type Key =
    { key: ConsoleKey
      modifier: ConsoleModifiers
      ch: char }

type SelectAction =
    | AddPathDir of dir: string
    | RemovePathDir
    | Select of dir: string
    | ToggleMode of wasOnDir: string
    | CycleSort of wasOnDir: string
    | SearchUpdate of newPattern: string
    | Cancel

type PickAction =
    | CancelPick
    | PickDirectory of fullpath: string

type Mode =
    | Navigate
    | Search of pattern: string

type SortMethod =
    | ByAccessed
    | ByModified
    | ByName

type Opt =
    {
        mode : Mode
        sort : SortMethod
    }


let styleBreadcrumb = stext [ bg Color.SlateGray; fg Color.MintCream ]
let styleSearchIndicator = stext [ fg Color.LightCyan ]
let styleSelected = stext [ bg Color.Peru; fg Color.MintCream ]
let styleUnselected = stext []
let styleSort = stext [fg Color.Tan]
let styleError = stext [ bg Color.Coral ]

let readkey () =
    let k = Console.ReadKey(true)

    { key = k.Key
      modifier = k.Modifiers
      ch = k.KeyChar }

let maxlines =
    ensureAvailableRows err 10

    Console.BufferHeight - 4

let listDirs sort path pattern =
    let dirs = Directory.GetDirectories(path, pattern)

    match sort with
    | ByName ->
        dirs
    | ByModified ->
        dirs |> Array.sortByDescending Directory.GetLastWriteTime
    | ByAccessed ->
        dirs |> Array.sortByDescending Directory.GetLastAccessTime

    |> Array.map Path.GetFileName

// FIXME not tail-calls
let rec pickFile opt preselect path =
    let mode = opt.mode
    let returnToParent () =
        let parent = Directory.GetParent path

        if parent = null then
            pickFile { opt with mode = Navigate} preselect path
        else
            let parentPath = parent.FullName
            let thisDir = Path.GetFileName path
            pickFile { opt with mode = Navigate } (Some thisDir) parentPath

    let dirs' =
        try
            Some(
                listDirs opt.sort path
                <| match mode with
                   | Navigate -> "*"
                   | Search pat -> $"*{pat}*"
            )
        with _ ->
            None

    match dirs' with
    | None ->
        err (styleError "Can't access directory")
        Threading.Thread.Sleep 1000
        err "\r"
        err (Operation.eraseLine)
        returnToParent ()

    | Some dirs ->
        err (formatPathBreadcrumbs path |> styleBreadcrumb)
        err "\n"

        let header, headerLinesCount =
            match opt with
            | { mode = Navigate; sort = ByName } -> "", 1
            | { mode = Navigate } ->
                styleSort <| sprintf "Sort (%A)" opt.sort
                + "\n"
                , 2
            | { mode = Search pattern } ->
                styleSort <| sprintf "Sort (%A)" opt.sort 
                + (styleSearchIndicator "Search: ")
                + pattern
                + "\n"
                , 2
        
        err header

        let rec showChoices idx showStartIdx maxShowCount =
            let idx = Math.Max(0, Math.Min(dirs.Length - 1, idx))

            let showStartIdx =
                let showLastIdx = showStartIdx + maxShowCount - 1

                if idx > showLastIdx then
                    showStartIdx + idx - showLastIdx
                else
                    Math.Min(idx, showStartIdx)

            let showEndIdx = Math.Min(dirs.Length, showStartIdx + maxShowCount)
            let showCount = showEndIdx - showStartIdx

            err "\r"

            [ showStartIdx .. showEndIdx - 1 ]
            |> List.map (fun i ->
                let dir = dirs[i]

                Operation.eraseLine + if i = idx then styleSelected dir else styleUnselected dir)
            |> String.concat sep
            |> err

            if showCount > 1 then
                // TODO does cursorUpLines 0 move cursor?
                err (Operation.cursorUpLines (showCount - 1))

            let back () =
                showChoices (idx - 1) showStartIdx maxShowCount

            let next () =
                showChoices (idx + 1) showStartIdx maxShowCount

            let child () =
                if dirs.Length > 0 then
                    AddPathDir dirs[idx]
                else
                    showChoices idx showStartIdx maxShowCount

            let parent () = RemovePathDir

            match mode, readkey () with
            | _,
              { key = ConsoleKey.C
                modifier = ConsoleModifiers.Control }
            | _, { key = ConsoleKey.Escape } -> Cancel
            | _, { key = ConsoleKey.UpArrow } -> back ()
            | _, { key = ConsoleKey.DownArrow } -> next ()
            | _, { key = ConsoleKey.RightArrow } -> child ()
            | _, { key = ConsoleKey.LeftArrow } -> parent ()
            | _,
              { key = ConsoleKey.Enter
                modifier = ConsoleModifiers.Control } -> Select path
            | _, { key = ConsoleKey.Enter } -> if dirs.Length = 0 then Select path else Select dirs[idx]
            | _, { ch = '/'; modifier = ConsoleModifiers.Alt } ->
                if dirs.Length > 0 then
                    CycleSort dirs[idx]
                else
                    CycleSort ""
            | _, { ch = '/' } ->
                if dirs.Length > 0 then
                    ToggleMode dirs[idx]
                else
                    ToggleMode ""
            | Navigate, { key = ConsoleKey.K } -> back ()
            | Navigate, { key = ConsoleKey.J } -> next ()
            | Navigate, { key = ConsoleKey.L } -> child ()
            | Navigate, { key = ConsoleKey.H } -> parent ()
            | Navigate, _ -> showChoices idx showStartIdx maxShowCount
            | Search _,
              { key = ConsoleKey.K
                modifier = ConsoleModifiers.Alt } -> back ()
            | Search _,
              { key = ConsoleKey.J
                modifier = ConsoleModifiers.Alt } -> next ()
            | Search _,
              { key = ConsoleKey.L
                modifier = ConsoleModifiers.Alt } -> child ()
            | Search _,
              { key = ConsoleKey.H
                modifier = ConsoleModifiers.Alt } -> parent ()
            | Search pat, { key = ConsoleKey.Backspace } -> SearchUpdate(pat[0 .. pat.Length - 2])
            | Search pat, k -> SearchUpdate(pat + string k.ch)

        let startIdx =
            match preselect with
            | None -> 0
            | Some name -> defaultArg (Array.tryFindIndex ((=) name) dirs) 0

        let selectAction = showChoices startIdx 0 maxlines


        err (Operation.cursorUpLines headerLinesCount)
        err (Operation.linesDelete <| dirs.Length + headerLinesCount)

        match selectAction with
        | Cancel -> CancelPick
        | AddPathDir dir -> pickFile { opt with mode = Navigate } None (addDir dir path)
        | RemovePathDir -> returnToParent ()
        | Select dir -> addDir dir path |> PickDirectory
        | ToggleMode lastDir ->
            match mode with
            | Navigate -> pickFile { opt with mode = ( Search "") }(Some lastDir) path
            | Search _ -> pickFile { opt with mode = Navigate } (Some lastDir) path
        | CycleSort lastDir ->
            let nextSort =
                match opt.sort with
                | ByName -> ByModified
                | ByModified -> ByAccessed
                | ByAccessed -> ByName
            pickFile { opt with sort = nextSort } (Some lastDir) path
        | SearchUpdate pat -> pickFile { opt with mode = ( Search pat) } preselect path

let opt =
    match Environment.GetCommandLineArgs() |> Array.tryItem 1 with
    | Some "-a" -> 
        {
            sort = ByAccessed
            mode = Navigate 
        }
    | Some "-m" -> 
        {
            sort = ByModified
            mode = Navigate 
        }
    | Some "--help"
    | Some "-h" ->
        printfn "%s" usage
        exit 1
    | Some a ->
        eprintfn "Unknown argument %s" a
        exit 1
    | None -> 
        {
            sort = ByName
            mode = Navigate 
        }

err (Operation.cursorHide)

let pickedFile = pickFile opt None Environment.CurrentDirectory

err (Operation.cursorShow)

match pickedFile with
| CancelPick -> Environment.CurrentDirectory
| PickDirectory fullpath -> fullpath
|> printfn "%s"
