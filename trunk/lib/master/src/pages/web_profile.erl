-module (web_profile).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
    case common_web:have_role([role_user, role_admin]) of
        true -> [];
        _ -> wf:redirect("/web/index")
    end,
    common_web:main().

title() ->
    common_web:title().

footer() ->
    common_web:footer(). 

get_info() ->
    common_web:get_info().

get_license() ->
    common_web:get_license().

menu() ->
    common_web:menu().

submenu() ->
    common_web:submenu().

user_status() ->
    common_web:is_logged_in(true).

body() ->
    {user, Username, _Password, Email, EmailNotification, _Something, _Role} = 
        db:get_user(wf:user()),
    Body = [
        #h2 { text="Profile" },
        #label { text="Username: " },
        #label { text=Username },
        #br{},
        #label { text="E-mail adress: " },
        #textbox { id=emailBox, text=Email },
        #p {},
        #checkbox { id=reciveEmail, text="I want to receive email when a job finishes", checked=EmailNotification },
        #p {},
        #button { text="Update", postback=updateInfo } 
    ],
    wf:render(Body).

event(updateInfo) ->
    [Email] = wf:q(emailBox),
    case wf:q(reciveEmail) of
        ["on"] -> ReceiveEmail = true;
        _ -> ReceiveEmail = false
    end,
    db:set_email(wf:user(), Email),
    db:set_email_notification(wf:user(), ReceiveEmail),
    wf:flash(wf:f("Your changes have been saved"));
event(logout) ->
    wf:clear_user(),
    wf:redirect("/web/login");
event(_) -> ok.
