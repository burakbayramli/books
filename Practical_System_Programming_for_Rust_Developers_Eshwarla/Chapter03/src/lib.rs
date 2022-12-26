// Standard library imports
use std::collections::HashMap;

// Each line in template file can be of one of following types
#[derive(PartialEq, Debug)]
pub enum ContentType {
    Literal(String),
    TemplateVariable(ExpressionData),
    Tag(TagType),
    Unrecognized,
}

//IF ContentType is TemplateVariable, the contents of the line are parsed and stored in the ExpressionData struct.alloc
// If template file contains this line: <p> Hello {{name}} ,welcome </p> then
// Head = "Hello" , variable = "name" and tail = ",welcome"
#[derive(PartialEq, Debug)]
pub struct ExpressionData {
    pub head: Option<String>,
    pub variable: String,
    pub tail: Option<String>,
}
#[derive(PartialEq, Debug)]
pub enum TagType {
    ForTag,
    IfTag,
}

//this checks if a symbol string is contained within another string
pub fn check_symbol_string(input: &str, symbol: &str) -> bool {
    input.contains(symbol)
}

// This checks for matching symbol strings in a given input
pub fn check_matching_pair(input: &str, symbol1: &str, symbol2: &str) -> bool {
    input.contains(symbol1) && input.contains(symbol2)
}

// this returns index of given char symbol, if symbol is present.
pub fn get_index_for_symbol(input: &str, symbol: char) -> (bool, usize) {
    let mut characters = input.char_indices();
    let mut does_exist = false;
    let mut index = 0;
    while let Some((c, d)) = characters.next() {
        if d == symbol {
            does_exist = true;
            index = c;
            break;
        }
    }
    (does_exist, index)
}

// Reads one line of template file and returns type of Content. The types of content are defined in ContentType enum
// If contentType is TemplateVariable, it parses the line further to separate out the head, variable and tail components.
// IF ContentType is Literal, it returns the read input without any modifications.

pub fn get_content_type(input_line: &str) -> ContentType {
    // Tag expressions are enclosed within {% and %}
    let is_tag_expression = check_matching_pair(&input_line, "{%", "%}");

    // ForTag expressions begin with  keywords 'for' and 'in' enclosed within {% and %}
    // ForTag expressions end with keyword 'endfor' enclosed within {% and %}
    let is_for_tag = (check_symbol_string(&input_line, "for")
        && check_symbol_string(&input_line, "in"))
        || check_symbol_string(&input_line, "endfor");
    // IfTag expressions begin with  keyword 'if' enclosed within {% and %}
    // IfTag expressions end with keyword 'endif' enclosed within {% and %}
    let is_if_tag =
        check_symbol_string(&input_line, "if") || check_symbol_string(&input_line, "endif");

    // Template variables have
    // 1) an optional head,
    // 2) a template variable enclosed within {{ and }}
    // 3) an optional tail
    // eg the expression <p> Hello {{name}} ,welcome </p> is parsed as follows:
    // head = 'Hello', variable = 'name' and tail = ',welcome'

    let is_template_variable = check_matching_pair(&input_line, "{{", "}}");
    let return_val;
    // case: For Tag
    if is_tag_expression && is_for_tag {
        return_val = ContentType::Tag(TagType::ForTag);
    //case: If Tag
    } else if is_tag_expression && is_if_tag {
        return_val = ContentType::Tag(TagType::IfTag);
    // case: Template variable
    } else if is_template_variable {
        let content = get_expression_data(&input_line);
        return_val = ContentType::TemplateVariable(content);
    // case: Literal
    } else if !is_tag_expression && !is_template_variable {
        return_val = ContentType::Literal(input_line.to_string());
    // Unknown type
    } else {
        return_val = ContentType::Unrecognized;
    }
    return_val
}

// Function to generate HTML for line containing template variable
pub fn generate_html_template_var(
    content: ExpressionData,
    context: HashMap<String, String>,
) -> String {
    let mut html = String::new();
    println!("expression data is:{:?}", content);
    if let Some(h) = content.head {
        html.push_str(&h);
    }

    if let Some(val) = context.get(&content.variable) {
        html.push_str(&val);
    }

    if let Some(t) = content.tail {
        html.push_str(&t);
    }

    html
}

// Helper function to parse template variable
pub fn get_expression_data(input_line: &str) -> ExpressionData {
    let (_h, i) = get_index_for_symbol(input_line, '{');
    let head = input_line[0..i].to_string();
    let (_j, k) = get_index_for_symbol(input_line, '}');
    let variable = input_line[i + 1 + 1..k].to_string();
    let tail = input_line[k + 1 + 1..].to_string();

    ExpressionData {
        head: Some(head),
        variable: variable,
        tail: Some(tail),
    }
}

//Unit tests
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn check_get_index_for_symbol_test() {
        assert_eq!((true, 3), get_index_for_symbol("Hi {name} bye", '{'));
    }
    #[test]
    fn check_template_var_test() {
        let content = ExpressionData {
            head: Some("Hi ".to_string()),
            variable: "name".to_string(),
            tail: Some(" bye".to_string()),
        };
        assert_eq!(
            ContentType::TemplateVariable(content),
            get_content_type("Hi {{name}} bye")
        );
    }
    #[test]
    fn check_for_tag_test() {
        assert_eq!(
            ContentType::Tag(TagType::ForTag),
            get_content_type("{% for name in names %} bye")
        );
    }
    #[test]
    fn check_if_tag_test() {
        assert_eq!(
            ContentType::Tag(TagType::IfTag),
            get_content_type("{% if name == 'Bob' %}")
        );
    }
    #[test]
    fn check_literal_test() {
        let s = "<h1>Hello world</h1>";
        assert_eq!(ContentType::Literal(s.to_string()), get_content_type(s));
    }

    #[test]
    fn check_get_expression_data_test() {
        let expression_data = ExpressionData {
            head: Some("Hi ".to_string()),
            variable: "name".to_string(),
            tail: Some(",welcome".to_string()),
        };

        assert_eq!(expression_data, get_expression_data("Hi {{name}},welcome"));
    }
    #[test]
    fn check_symbol_string_test() {
        assert_eq!(true, check_symbol_string("{{Hello}}", "{{"));
    }
    #[test]
    fn check_symbol_pair_test() {
        assert_eq!(true, check_matching_pair("{{Hello}}", "{{", "}}"));
    }
}
