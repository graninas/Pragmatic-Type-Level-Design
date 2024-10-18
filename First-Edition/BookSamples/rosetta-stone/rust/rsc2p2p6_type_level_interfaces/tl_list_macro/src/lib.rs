extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use core::str::Chars;

// #[proc_macro]
// pub fn tl_list(input: TokenStream) -> TokenStream {
//     let input_string = input.to_string();

//     let mut chs: Chars<'_> = input_string.chars();

//     let mut constr = quote!{N_};

//     let front_opt_ch = chs.next();
//     let back_opt_ch = chs.next_back();

//     if (front_opt_ch == back_opt_ch)
//         && (front_opt_ch == Some('\"'))
//     {
//       let mut next_ch = chs.next_back();
//       while next_ch.is_some() {
//         let ch = next_ch.expect("Invalid char");
//         constr = quote!{C_<#ch, #constr>};
//         next_ch = chs.next_back();
//       }
//     }
//     else {
//       panic!("Type-level string must be enclosed with double quotes.");
//     }

//     TokenStream::from(constr)
// }
