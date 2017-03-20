{user_id, user_pwd}:
(import ./.) {
  zone = "dev";
  salt-user = "${user_id}";
  salt-pass = "${user_pwd}";
  salt-url = "https://salt.dev.srv.cirb.lan:8000";
}
