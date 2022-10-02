extern crate cargo_metadata;
extern crate glob;

use std::{
    env,
    error::Error,
    fs::File,
    io::{BufReader, BufWriter, Read, Write},
    path::Path,
};

use cargo_metadata::MetadataCommand;
use glob::glob;

fn main() -> Result<(), Box<dyn Error>> {
    // Search compile message source files with the name `*_message.txt.in`
    for entry in glob("src/*_message.txt.in").expect("Failed to read glob pattern") {
        let name: String = entry?
            .file_stem()
            .unwrap() // filename must contain `.`
            .to_str()
            .expect("Invalid file name")
            .strip_suffix("_message.txt")
            .unwrap() // file stem must end in `_message.txt`
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
    let mut inbuf = BufReader::new(File::open(format!("src/{}_message.txt.in", name))?);

    let out_dir = env::var("OUT_DIR")?;
    let dest_path = Path::new(&out_dir).join(format!("{}_message.txt", name));
    let mut outbuf = BufWriter::new(File::create(&dest_path)?);

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
        .into_iter().find(|pkg| pkg.name == env::var("CARGO_PKG_NAME").unwrap())
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
