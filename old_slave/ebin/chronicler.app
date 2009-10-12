{application, chronicler,
    [{description, "Chronicler - local logging subsystem"},
    {vsn, "0.1"},
    {modules, [chronicler, chronicler_sup, chronicler_server]},
    {registered, [chronicler_server]},
    {applications, [kernel, stdlib]},
    {mod, {chronicler, []}}
    ]}.
