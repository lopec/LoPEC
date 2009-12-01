-module (web_register).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"LoPEC".

footer() ->
    "LoPEC 2009".

get_info() ->
    "Low Power Erlang-based Cluster".

get_license() ->
"The hallmark of proprietary software licenses is that the software publisher grants a license to use one or more copies of software, but that ownership of those copies remains with the software publisher (hence use of the term proprietary). One consequence of this feature of proprietary software licenses is that virtually all rights regarding the software are reserved by the software publisher. Only a very limited set of well-defined rights are conceded to the end-user. Therefore, it is typical of proprietary software license agreements to include many terms which specifically prohibit certain uses of the software, often including uses which would otherwise be allowed under copyright law.".

body() ->
    Body = [
    #h3 {text="User information"},
    #label {text="Desired username: "},
    #textbox {id=username},
    #p {},
    #label {text="Password: "},
    #password {id=password1},
    #p {},
    #label {text="Retype password: "},
    #password {id=password2},
    #p {},
    #label {text="Email adress: "},
    #textbox {id=email},
    #p {},
    #label {text="End user license agreement?"},
    #panel { id=licensePanel, style="overflow: auto; width: 500px; border: 1px solid black; height: 200px;", body=[get_license()]},
    #checkbox { text="I agree with the eula", checked=false },
    #p {},
    #button {id=registerButton, text="Register", postback=continue}

    ],
    wf:wire(registerButton, username, #validate { validators=[
        #is_required { text="Required." }
    ]}),
    wf:wire(registerButton, password2, #validate { validators=[
        #confirm_password { text="Passwords must match.", password=password1 }
    ]}),
    wf:wire(registerButton, email, #validate { validators=[
        #is_email { text="Not a valid email address."}
    ]}),

    wf:render(Body).

event(continue) ->
    %% Here we register the user
    [Username] = wf:q(username),
    [Password] = wf:q(password1),
    [Email] = wf:q(email),
    db:add_user(Username, Email, Password),
    wf:redirect("login");
event(_) -> ok.
