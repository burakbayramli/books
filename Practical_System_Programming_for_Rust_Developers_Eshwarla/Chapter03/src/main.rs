use std::collections::HashMap;
use std::io;
use std::io::BufRead;
use template_engine::*;

fn main() {
    let mut context: HashMap<String, String> = HashMap::new();
    context.insert("name".to_string(), "Bob".to_string());
    context.insert("city".to_string(), "Boston".to_string());

    for line in io::stdin().lock().lines() {
        match get_content_type(&line.unwrap().clone()) {
            ContentType::TemplateVariable(content) => {
                let html = generate_html_template_var(content, context.clone());
                println!("{}", html);
            }
            ContentType::Literal(text) => println!("{}", text),
            ContentType::Tag(TagType::ForTag) => println!("For Tag not implemented"),
            ContentType::Tag(TagType::IfTag) => println!("If Tag not implemented"),
            ContentType::Unrecognized => println!("Unrecognized input"),
        }
    }
}
