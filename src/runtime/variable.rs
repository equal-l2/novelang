use crate::types::Typed;

#[derive(Debug, Clone)]
pub struct Variable {
    is_mutable: bool,
    value: Typed,
}

#[derive(Debug)]
pub enum ModifyError {
    TypeDiffers,
    Immutable,
}

impl Variable {
    pub const fn new(value: Typed) -> Self {
        Self {
            is_mutable: false,
            value,
        }
    }

    pub const fn new_mut(value: Typed) -> Self {
        Self {
            is_mutable: true,
            value,
        }
    }

    pub fn modify(&mut self, to: Typed) -> Result<Typed, ModifyError> {
        if self.is_mutable {
            match (&self.value, &to) {
                (Typed::Num(_), Typed::Num(_)) | (Typed::Bool(_), Typed::Bool(_)) => {
                    Ok(std::mem::replace(&mut self.value, to))
                }
                _ => Err(ModifyError::TypeDiffers),
            }
        } else {
            Err(ModifyError::Immutable)
        }
    }

    pub fn force_modify(&mut self, to: Typed) -> Typed {
        match (&self.value, &to) {
            (Typed::Num(_), Typed::Num(_)) | (Typed::Bool(_), Typed::Bool(_)) => {
                std::mem::replace(&mut self.value, to)
            }
            _ => panic!("type differs in force_modify"),
        }
    }

    pub const fn get(&self) -> &Typed {
        &self.value
    }
}
