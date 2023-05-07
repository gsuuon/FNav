# FNav
F# Navigator

Simple directory navigator that outputs final selection to stdout, letting you `cd (fn)` or `fn | cd $in`

https://user-images.githubusercontent.com/6422188/236075604-7f1aef58-4c4a-4917-9179-3376cc451171.mp4

## Usage
```
cd (fn)
```

`j` `k` `↓` `↑` - Select down / up  
`h` `l` `←` `→` - Go to parent / Enter dir  
`<enter>` - Pick selected directory  
`<ctrl-enter>` - Pick parent directory  
`<esc>` - Pick original directory  
`/` - Toggle Search mode  
`<alt-h>` `<alt-l>` `<alt-j>` `<alt-k>` - Search mode select / change dirs  
`<alt-/>` - Cycle sort-by


## Install
From nuget:
```
dotnet tool install --global gsuuon.tool.fnav
```
From repo:
```
dotnet pack
dotnet tool install --global --add-source ./nupkg --no-cache gsuuon.tool.fnav

```
### To alias
nushell
```nu
def-env fn [] {
  cd (^fn)
}
```
bash
```bash
alias fn='cd $(\fn)'
```
powershell
```powershell
function fn { cd (fn.exe) }
```
