{user_id, user_pwd}:
(import ./.) {
  zone = "prod";
  salt-user = "${user_id}";
  salt-pass = "${user_pwd}";
  salt-url = "https://salt.prd.srv.cirb.lan:8000";
}
