extern crate cargo_metadata;
extern crate glob;

use std::{
    env,
    error::Error,
    fs::{self, File},
    io::{BufReader, BufWriter, Read, Write},
    path::Path,
};

use cargo_metadata::MetadataCommand;
use glob::glob;

fn main() -> Result<(), Box<dyn Error>> {
    // Search for and compile message source files
    for entry in glob("src/messages/*.in").expect("Failed to read glob pattern") {
        let name: String = entry?
            .file_name()
            .unwrap() // filename must exist
            .to_str()
            .expect("Invalid UTF-8 in file name")
            .strip_suffix(".in")
            .unwrap() // file name must end in `.in`
            .to_owned();
        compile_message(&name)?;
    }
    Ok(())
}

macro_rules! replace_with_env {
    ( $buf:expr; $( $a:literal -> $b:literal ),+ $(,)? ) => {
        $( $buf = $buf.replace(concat!("%",$a,"%"), &env::var($b)?); )+
    };
}

fn compile_message(name: &str) -> Result<(), Box<dyn Error>> {
    let mut inbuf = BufReader::new(File::open(format!("src/messages/{}.in", name))?);

    let cargo_out_dir_name = env::var("OUT_DIR")?;
    let dest_dir = Path::new(&cargo_out_dir_name).join("messages");
    let out_file = dest_dir.join(name);

    // Create destination file and parent directory
    fs::create_dir_all(dest_dir)?; // create_dir_all() doesn't fail if dir exists
    let mut outbuf = BufWriter::new(File::create(&out_file)?);

    // Read message source
    let mut message = String::new();
    inbuf
        .read_to_string(&mut message)
        .expect("Invalid UTF-8 in message source");

    // Perform simple substitutions
    replace_with_env! { message;
        "PROGNAME" -> "CARGO_PKG_NAME",
        "VERSION" -> "CARGO_PKG_VERSION",
    };

    // Perform author substitution
    let authors: String = env::var("CARGO_PKG_AUTHORS")?;
    let firstauthor: String = authors.chars().take_while(|c| *c != ':').collect();
    message = message.replace("%PRIMARY_AUTHOR%", &firstauthor);

    // Perform copyright year substitution.
    // Cargo.toml must contain:
    //
    //     [package.metadata]
    //     copyright-years = "..."
    //
    let years = MetadataCommand::new()
        .exec()?
        .packages
        .into_iter()
        .find(|pkg| pkg.name == env::var("CARGO_PKG_NAME").unwrap())
        .unwrap()
        .metadata["copyright-years"]
        .as_str()
        .expect("Can't find `package.metadata.copyright-years` in Cargo.toml")
        .to_owned();
    message = message.replace("%YEARS%", &years);

    // Write output message
    outbuf.write_all(message.as_bytes())?;

    Ok(())
}
