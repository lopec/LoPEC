-module (web_register).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
    common_web:main().

title() ->
    common_web:title().

footer() ->
    common_web:footer(). 

get_info() ->
    common_web:get_info().

get_license() ->
    common_web:get_license().

body() ->
    Body = [
    #h3 {text="User information"},
    #label {text="Desired username: "},
    #textbox {id=username, next=password1},
    #p {},
    #label {text="Password: "},
    #password {id=password1, next=password2},
    #p {},
    #label {text="Retype password: "},
    #password {id=password2, next=email},
    #p {},
    #label {text="Email adress: "},
    #textbox {id=email, next=registerButton},
    #p {},
    #label {text="End user license agreement?"},
    #panel { id=licensePanel, style="overflow: auto; width: 500px; border: 1px solid black; height: 200px;", body=[get_license()]},
    #checkbox { text="I agree with the eula", checked=false },
    #p {},
    #button {id=registerButton, text="Register", postback=continue}

    ],
    wf:wire(registerButton, username, #validate { validators=[
        #is_required { text="Required." },
        #custom { text="Username is already taken.", function=fun username_free/2, tag=check }
    ]}),
    wf:wire(registerButton, password2, #validate { validators=[
        #confirm_password { text="Passwords must match.", password=password1 }
    ]}),
    wf:wire(registerButton, email, #validate { validators=[
        #is_email { text="Not a valid email address."}
    ]}),

    wf:render(Body).

username_free(_Tag, Username) ->
    case db:exist_user(Username) of
        {ok, no} -> true;
        {ok, yes} -> false
    end.

event(continue) ->
    %% Here we register the user
    [Username] = wf:q(username),
    [Password] = wf:q(password1),
    [Email] = wf:q(email),
    db:add_user(Username, Email, Password),
    wf:redirect("login");
event(_) -> ok.
