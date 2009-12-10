-module(web_admin_handle_users).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).


main() ->
    %% Only admins are supposed to see this page
    case common_web:have_role([role_admin]) of
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
        #tablecell { body=[
            #dropdown { id="roles"++Username, postback={change_role, Username},
                options=[
                #option { text="no role", value="undefined", selected=undefined==Role },
                #option { text="User", value="role_user", selected=role_user==Role },
                #option { text="Administrator", value="role_admin", selected=role_admin==Role }
            ]}
        ]},
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
event({change_role, Username}) ->
    [NewRole]=wf:q("roles"++Username),
    NewRoleAtom = list_to_atom(NewRole),
    case db:set_role(Username, NewRoleAtom) of 
        {ok, _} -> 
            wf:flash(wf:f("Updated " ++ Username ++ " with new role"));
        {error, _} -> 
            wf:flash(wf:f("Could not update " ++ Username ++ " role. Try again"))
    end;
event(logout) ->
    wf:clear_user(),
    wf:redirect("/web/login"); 
event(_) -> ok.

