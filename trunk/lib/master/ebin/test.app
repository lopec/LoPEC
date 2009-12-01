{application, test, [
	{description,  "Nitrogen Website"},
	{mod, {test_app, []}},
    {vsn, "1.0"},
    {modules, []},
    {registered, []},
    {applications, [kernel, stdlib, chronicler, ecg, common]},
	{env, [
		{platform, inets}, %% {inets|yaws|mochiweb}
		{port, 8000},
		{session_timeout, 20},
		{sign_key, "SIGN_KEY"},
		{www_root, "./wwwroot"}
	]}
]}.
