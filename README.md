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

## Install
`nu install.nu` or run what's in the file in your shell of choice
