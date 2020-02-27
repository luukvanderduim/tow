use super::*;

use clap::{crate_version, App, Arg};

pub(crate) fn quarrel(cts: &Arc<CaretTowState>) {
    let matches = App::new("Tow")
        .version(crate_version!())
        .author("Luuk van der Duim <luukvanderduim@gmail.com>")
        .arg(
            Arg::with_name("daemon")
                .short("D")
                .long("daemon")
                .takes_value(false)
                .help("Have tow be 'daemonized' / run in the background."),
        )
        .arg(
            Arg::with_name("behavior")
                .short("b")
                .long("behavior")
                .takes_value(true)
                .help("Mode: pulse [N: 100-10000] (ms) or typewriter (default)")
                .max_values(2),
        )
        .get_matches();

    if matches.is_present("daemon") {
        spookify_tow();
    }

    if matches.is_present("behavior") {
        let mut bvals = matches.values_of("behavior").expect("Unexpected!");
        match bvals.next() {
            Some("pulse") => {
                if let Some(numb) = bvals.next() {
                    let n: u16 = numb.parse::<u16>().expect("u16 parse error");
                    if n <= 99 {
                        cts.set_behavior(Behavior::Pulse {
                            dur: Duration::from_millis(100),
                        });
                    }
                    if n >= 100 && n <= 10000 {
                        cts.set_behavior(Behavior::Pulse {
                            dur: Duration::from_millis(u64::from(n)),
                        });
                    } else {
                        cts.set_behavior(Behavior::Pulse {
                            dur: Duration::from_millis(2000),
                        });
                    }
                }
            }
            Some("typewriter") => {
                cts.set_behavior(Behavior::Typewriter);
            }
            Some(&_) => {
                eprintln!("Error: Invalid 'behavior' value. Typo?");
            }
            None => {
                eprintln!("Error: Invalid 'behavior'  value.");
            }
        }
    }
}
