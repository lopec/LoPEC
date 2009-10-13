{application, chronicler,
    [{description, "Clusterbusters logger application"},
    {vsn, "0.1"},
    {modules, [
        chronicler_app,
        chronicler_sup,
        chronicler
            ]},
    {registered, []},
    {applications, [kernel, stdlib]},
    {mod, {chronicler_app,[]}}
    ]}.
