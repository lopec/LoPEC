{application, mainChronicler,
    [{description, "Clusterbusters main logger application"},
    {vsn, "0.1"},
    {modules, [
        mainChronicler_app,
        mainChronicler_sup,
        main_chronicler
            ]},
    {registered, [externalLoggerPID]},
    {applications, [kernel, stdlib]},
    {mod, {mainChronicler_app,[]}}
    ]}.
