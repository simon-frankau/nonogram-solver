use assert_cmd::Command;
use predicates::prelude::*;

// Going to keep it light: Just test the happy path and a simple
// parser failure.

#[test]
fn test_cli_success() {
   let expected = std::fs::read_to_string("results/traffic_light_res.txt")
       .unwrap();

   let mut cmd = Command::cargo_bin("nonogram-solver").unwrap();

   cmd.pipe_stdin("examples/traffic_light.txt")
       .unwrap()
       .assert()
       .success()
       .stdout(expected);
}

#[test]
fn test_cli_success_multi() {
   let expected = std::fs::read_to_string("results/multi_res.txt")
       .unwrap();

   let mut cmd = Command::cargo_bin("nonogram-solver").unwrap();

   cmd.pipe_stdin("examples/multi.txt")
       .unwrap()
       .assert()
       .success()
       .stdout(expected);
}

#[test]
fn test_cli_failure() {
   let mut cmd = Command::cargo_bin("nonogram-solver").unwrap();

   cmd.write_stdin("This is not a valid input.")
       .assert()
       .failure()
       .stdout("")
       .stderr(predicate::str::contains("Expected three sections"));
}
