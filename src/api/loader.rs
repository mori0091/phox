use super::*;

fn core_assets_dir() -> std::path::PathBuf {
    if let Ok(exe_path) = std::env::current_exe() {
        if let Some(dir) = exe_path.parent() {
            let mut dir = dir.to_path_buf();
            dir.push("assets");
            if dir.is_dir() {
                return dir
            }
        }
    }
    let mut dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dir.push("assets");
    assert!(dir.is_dir(), "core assets directory not found");
    dir
}

fn user_assets_dir() -> std::path::PathBuf {
    if let Ok(dir) = std::env::current_dir() {
        return dir
    }
    panic!("user assets directory not found")
}

pub fn load_path() -> &'static Vec<std::path::PathBuf> {
    static CACHE: std::sync::LazyLock<Vec<std::path::PathBuf>>
        = std::sync::LazyLock::new(|| vec![core_assets_dir(), user_assets_dir()]);
    &*CACHE
}

pub fn load_module_src(path: &Path) -> Result<(std::path::PathBuf, String), Error> {
    match path {
        Path::Relative(_) => {
            let msg = format!("ambiguous module path `{}`", path.pretty());
            Err(Error::Message(msg))
        }
        Path::Absolute(ps) => {
            for mut filepath in load_path().iter().cloned() {
                for p in ps {
                    if let PathComponent::Name(m) = p {
                        filepath.push(m)
                    }
                    else {
                        let msg = format!("illegal module path `{}`", path.pretty());
                        return Err(Error::Message(msg))
                    }
                }
                filepath.set_extension("phx");
                if filepath.is_file() {
                    match std::fs::read_to_string(&filepath) {
                        Ok(src) => {
                            return Ok((filepath, src))
                        }
                        Err(e) => {
                            let msg = format!("module `{}` load failed: {:?}", path.pretty(), e);
                            return Err(Error::Message(msg))
                        }
                    }
                }
            }
            let msg = format!("module not found `{}` in {:?}", path.pretty(), load_path());
            Err(Error::Message(msg))
        }
    }
}
