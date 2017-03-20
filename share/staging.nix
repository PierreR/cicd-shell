{user_id, user_pwd}:
(import ./.) {
  zone = "staging";
  salt-user = "${user_id}";
  salt-pass = "${user_pwd}";
  salt-url = "https://salt.sta.srv.cirb.lan:8000";
}
