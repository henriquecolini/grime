use grime::{parser::Module, semantic::resolver::ResolvedModule};

fn main() {
	let arg = std::env::args().nth(1).expect("Expected path to module");
	let module = match Module::open(arg) {
		Ok(m) => m,
		Err(e) => {
			match e {
				grime::parser::Error::IO(e) => {
					eprintln!("{}", e);
				}
				grime::parser::Error::Filename => {
					eprintln!("Invalid filename");
				}
				grime::parser::Error::Parse(e) => {
					eprintln!("{}", e);
				}
			}
			return;
		}
	};
	let _resolved = match ResolvedModule::resolve(module) {
		Ok(r) => r,
		Err(e) => {
			eprintln!("{}", e);
			return;
		}
	};
}
