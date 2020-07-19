Config { font = "xft:Montserrat:Medium:size=12"
       , additionalFonts = [ "xft:iosevka:Medium:size=12" ]
       , borderColor = "black"
       , border = TopB
       , bgColor = "black"
       , fgColor = "#a0a0a0"
       , alpha = 192
       , position = Top
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = "."
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run Weather "ESGG" ["-t", "<station>: <tempC>C", "-L", "18", "-H", "25", "--normal", "#badc58", "--high", "#eb4d4b", "--low", "#686de0"] 36000
                    , Run Date "%Y-%m-%d %a %H:%M" "date" 10
                    , Run StdinReader
                    , Run Com "/home/jco/.local/bin/get-pulse-volume.sh" [] "vol" 1
                    , Run Com "uname" ["-r"] "" 0
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader%"
       }
