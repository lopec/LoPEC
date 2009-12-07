-module(web_admin_handle_users).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).


main() ->
    %% Only admins are supposed to see this page
    common_web:have_role([role_admin]),
    common_web:main().

title() ->
    common_web:title().

footer() ->
    common_web:footer().

get_info() ->
    common_web:get_info().

menu() ->
    common_web:menu().

submenu() ->
    common_web:submenu().

user_status() ->
    common_web:is_logged_in(true).


get_user_row([]) -> [];
get_user_row([{Username, Email, Role}|T]) ->
    [#tablerow { cells=[
        #tablecell { text=Username },
        #tablecell { text=Email },
        #tablecell { text=Role },
        #tablecell { body=[
            #link { postback={remove, Username}, body=#image { image="/images/delete.gif", style="border: 0px;" } } ] }
    ]}|get_user_row(T)].

get_user_table() ->
    {ok, UserList} = db:list_users(),
    #table { id=userTable ,class="jobtable", rows=[
        #tablerow { cells=[
            #tableheader { text="Username" },
            #tableheader { text="Email" },
            #tableheader { text="Role" },
            #tableheader { text="Control" }
        ]}|get_user_row(UserList)
    ]}.


body() ->
    Body = [
        #h2 { text="Modify users" },
        get_user_table()
    ],
    wf:render(Body).


event({remove, Username}) ->
    db:delete_user(Username),
    wf:update(userTable, get_user_table()),
    wf:comet_flush();
event(_) -> ok.

