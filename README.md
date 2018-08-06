# CICD Shell


The cicd shell is command line tool that interfaces cicd services such as the `orchestration` salt framework.

It is a tiny command line wrapper against [pepper](https://github.com/saltstack/pepper) written in [Haskell](https://haskell-lang.org/).

It provides specific CICD shortcuts and a more general console where all salt commands can be executed.

Users will need to be registered in their relevant AD groups to gain access to the Salt APIs for the targeted machines.

To know more, please consult http://docs.cicd.cirb.lan/shell/
