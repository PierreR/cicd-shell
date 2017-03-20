{user_id, user_pwd}:
(import ./.) {
  zone = "sandbox";
  salt-user = "${user_id}";
  salt-pass = "${user_pwd}";
  salt-url = "https://saltmaster.sandbox.srv.cirb.lan:8000";
}
