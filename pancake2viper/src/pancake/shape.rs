use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "shape.pest"]
struct ShapeParser;

#[derive(Debug, Clone)]
pub enum Shape {
    Simple(u64),
    Nested(Vec<Shape>),
}

pub fn parse_shape(s: &str) -> anyhow::Result<Shape> {
    // if s == "1" {
    //     return Ok(Shape::Simple(1));
    // }
    // let mut counter = 0;
    // let mut stack = vec![];
    // for c in s.chars() {
    //     match c {
    //         '<' => {
    //             counter = 0;
    //             stack.push(vec![]);
    //         }
    //         '1' => counter += 1,
    //         ',' => (),
    //         '>' => ,
    //         '_' => panic!(),
    //     }
    // }
    // todo!()
    let top = ShapeParser::parse(Rule::TOP, s)?;
    let mut shape = traverse_shape(top);
    reduce_shape(&mut shape);
    Ok(shape)
}

fn traverse_shape(pairs: pest::iterators::Pairs<Rule>) -> Shape {
    let mut counter = 0;
    // println!("Called traverse: {:?}", pairs);
    let mut buf = vec![];
    for pair in pairs {
        match pair.as_rule() {
            Rule::NESTED => {
                if counter != 0 {
                    buf.push(Shape::Simple(counter));
                    counter = 0;
                }
                let nested = traverse_shape(pair.into_inner());
                buf.push(nested);
            }
            _ => {
                counter += 1;
            }
        }
    }
    if buf.is_empty() {
        Shape::Simple(counter)
    } else {
        if counter != 0 {
            buf.push(Shape::Simple(counter));
        }
        Shape::Nested(buf)
    }
}

fn reduce_shape(shape: &mut Shape) {
    match shape {
        Shape::Nested(vec) if vec.len() == 1 => {
            if let Shape::Simple(x) = vec[0] {
                *shape = Shape::Simple(x)
            }
        }
        Shape::Nested(vec) => {
            for entry in vec {
                reduce_shape(entry);
            }
        }
        _ => (),
    }
}
