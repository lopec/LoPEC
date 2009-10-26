{application, ecg,
    [{description, "ECG - cluster heartbeat monitoring"},
    {vsn, "0.2"},
    {modules, [ecg, ecg_sup, ecg_server]},
    {registered, [ecg_server]},
    {applications, [kernel, stdlib, chronicler]},
    {mod, {ecg, []}}
    ]}.
