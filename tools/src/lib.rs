use std::fs::File;
use std::io::Result;
use std::io::{BufWriter, Write};
use std::path::PathBuf;

pub fn file_or_stdout(filename: &Option<PathBuf>) -> Result<Box<dyn Write>> {
    filename
        .as_ref()
        .map::<Result<Box<dyn Write>>, _>(|path| Ok(Box::new(BufWriter::new(File::create(path)?))))
        .unwrap_or_else(|| Ok(Box::new(std::io::stdout())))
}
