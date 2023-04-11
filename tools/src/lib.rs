use std::fs::File;
use std::io::{BufReader, BufWriter, Write};
use std::io::{Read, Result};
use std::path::PathBuf;

pub fn file_or_stdout(filename: &Option<PathBuf>) -> Result<Box<dyn Write>> {
    filename
        .as_ref()
        .map::<Result<Box<dyn Write>>, _>(|path| Ok(Box::new(BufWriter::new(File::create(path)?))))
        .unwrap_or_else(|| Ok(Box::new(std::io::stdout())))
}

pub fn file_or_stdin(filename: &Option<PathBuf>) -> Result<Box<dyn Read>> {
    filename
        .as_ref()
        .map::<Result<Box<dyn Read>>, _>(|path| Ok(Box::new(BufReader::new(File::open(path)?))))
        .unwrap_or_else(|| Ok(Box::new(std::io::stdin())))
}
