open System
open System.IO
open System.Drawing

open Gsuuon.Console.Buffer
open Gsuuon.Console.Vterm
open Gsuuon.Console.Style

let sep = "\n"
let pathSep = string Path.DirectorySeparatorChar

let selected = Color.Tan

let formatPathBreadcrumbs (path: string) =
    let home = Environment.SpecialFolder.UserProfile |> Environment.GetFolderPath

    path.Replace(home, "~").Replace(pathSep, " > ")

let listDirs path =
    Directory.GetDirectories path |> Array.map Path.GetFileName

let addDir path parent = Path.Combine [| parent; path |]

type Key =
    { key: ConsoleKey
      modifier: ConsoleModifiers }

let readkey () =
    let k = Console.ReadKey(true)

    { key = k.Key; modifier = k.Modifiers }

type SelectAction =
    | AddPathDir of dir: string
    | RemovePathDir
    | Select of dir: string
    | Cancel

type PickAction =
    | CancelPick
    | PickDirectory of fullpath: string

let rec pickFile preselect path =
    let dirs = listDirs path

    formatPathBreadcrumbs path |> stext [ bg Color.SlateGray ] |> err
    err "\n"

    let rec showChoices idx =
        let idx = Math.Max(0, Math.Min(dirs.Length - 1, idx))

        dirs
        |> Array.iteri (fun i dir ->
            if i = idx then err (stext [ bg selected ] dir) else err dir

            err sep)

        err (Operation.cursorUpLines dirs.Length)

        match readkey () with
        | { key = ConsoleKey.C
            modifier = ConsoleModifiers.Control }
        | { key = ConsoleKey.Escape } -> Cancel
        | { key = ConsoleKey.UpArrow }
        | { key = ConsoleKey.K } -> showChoices (idx - 1)
        | { key = ConsoleKey.DownArrow }
        | { key = ConsoleKey.J } -> showChoices (idx + 1)
        | { key = ConsoleKey.RightArrow }
        | { key = ConsoleKey.L } -> AddPathDir(dirs[idx])
        | { key = ConsoleKey.LeftArrow }
        | { key = ConsoleKey.H } -> RemovePathDir
        | { key = ConsoleKey.Enter } -> Select(dirs[idx])
        | _ -> showChoices idx


    let startIdx =
        match preselect with
        | None -> 0
        | Some name -> defaultArg (Array.tryFindIndex ((=) name) dirs) 0

    let selectAction = showChoices startIdx

    err (Operation.linesDelete dirs.Length)
    err (Operation.cursorUpLines 1)
    err (Operation.linesDelete 1)

    match selectAction with
    | Cancel -> CancelPick
    | AddPathDir dir -> addDir dir path |> pickFile None
    | RemovePathDir ->
        let parent = (Directory.GetParent path).FullName
        let thisDir = Path.GetFileName path
        pickFile (Some thisDir) parent
    | Select dir -> addDir dir path |> PickDirectory

err (Operation.cursorHide)

let pickedFile = pickFile None Environment.CurrentDirectory

err (Operation.cursorShow)

match pickedFile with
| CancelPick -> Environment.CurrentDirectory
| PickDirectory fullpath -> fullpath
|> printfn "%s"
