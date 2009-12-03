-module (web_login).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
    common_web:main().

title() ->
    common_web:title().

%% This is not currently used in our design
footer() ->
    common_web:footer().

get_info() ->
    common_web:get_info().

% Creates the menu.
menu() ->
    #panel { id=menuitem, body=[
        #link { text="Register", url="register" }
    ]}.
 
body() ->
    Body = [
        #panel { id=login, body=[
            #label { text="Username: " },
            #textbox { id=username, next=password },
            #p {},
            #label { text="Password: " },
            #password { id=password, next=loginButton },
            #panel { id=text, body=[
                "If you have forgot your password [press here]"
            ] },
            #p {},
            #button { id=loginButton, text="Login", postback=continue }
        ]}
    ],
    %% We hotwire some properties to our textboxes (Activated when button
    %% is pressed)
    wf:wire(loginButton, username, #validate { validators=[
        #is_required{text="Required."}]}),
    wf:wire(loginButton, password, #validate { validators=[
        #is_required{text="Required."}]}),
    wf:render(Body).

%% This event is fired up when the login-button is pressed
event(continue) ->
    case db:validate_user(hd(wf:q(username)), hd(wf:q(password))) of 
        {ok, user_validated} ->
            wf:user(hd(wf:q(username))),
            wf:redirect_from_login("index");
        _ ->
            wf:flash(wf:f("Wrong username and/or password"))
    end;
event(_) -> 
    ok.
