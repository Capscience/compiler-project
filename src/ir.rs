type IRVar = String;
type Label = String;

pub enum Instruction {
    LoadBoolConst {
        value: bool,
        dest: IRVar,
    },
    LoadIntConst {
        value: i64,
        dest: IRVar,
    },
    Copy {
        source: IRVar,
        dest: IRVar,
    },
    Call {
        fun: IRVar,
        args: Vec<IRVar>,
        dest: IRVar,
    },
    Jump {
        label: Label,
    },
    CondJump {
        cond: IRVar,
        then_label: Label,
        else_label: Label,
    },
    Label {
        name: Label,
    },
}

impl ToString for Instruction {
    fn to_string(&self) -> String {
        match self {
            Self::LoadBoolConst { value, dest } => format!("LoadBoolConst({value}, {dest})"),
            Self::LoadIntConst { value, dest } => format!("LoadIntConst({value}, {dest})"),
            Self::Copy { source, dest } => format!("Copy({source}, {dest})"),
            Self::Call { fun, args, dest } => {
                let mut string = format!("Call({fun}, [");
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        string.push_str(", ")
                    }
                    string.push_str(arg.as_str());
                }
                string.push_str(format!("], {dest})").as_str());
                string
            }
            Self::Jump { label } => format!("Jump({label})"),
            Self::CondJump {
                cond,
                then_label,
                else_label,
            } => format!("CondJump({cond}, {then_label}, {else_label})"),
            Self::Label { name } => name.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_string() {
        assert_eq!(
            Instruction::LoadIntConst {
                value: 1,
                dest: "x1".to_string()
            }
            .to_string(),
            "LoadIntConst(1, x1)"
        );
        assert_eq!(
            Instruction::LoadBoolConst {
                value: false,
                dest: "x1".to_string()
            }
            .to_string(),
            "LoadBoolConst(false, x1)".to_string()
        );
        assert_eq!(
            Instruction::Call {
                fun: "print_int".to_string(),
                args: vec!["a".into(), "b".into()],
                dest: "x1".into()
            }
            .to_string(),
            "Call(print_int, [a, b], x1)".to_string()
        );
    }
}
