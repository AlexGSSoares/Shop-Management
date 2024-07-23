open Str

(* Parsing functions *)
let read_file filename =
  let lines = ref [] in
  let channel = open_in filename in
  try
    while true do
      lines := input_line channel :: !lines
    done; []
  with End_of_file ->
    close_in channel;
    List.rev !lines

let parse_discount line =
  let regexp = Str.regexp "discount(\\(.*\\), \\(.*\\))." in
  if Str.string_match regexp line 0 then
    Some (Str.matched_group 1 line, float_of_string (Str.matched_group 2 line))
  else
    None

let parse_item line =
  let regexp = Str.regexp "item(\\(.*\\), '\\(.*\\)', '\\(.*\\)', \\(.*\\), \\(.*\\))." in
  if Str.string_match regexp line 0 then
    Some (int_of_string (Str.matched_group 1 line), Str.matched_group 2 line, Str.matched_group 3 line, float_of_string (Str.matched_group 4 line), int_of_string (Str.matched_group 5 line))
  else
    None

let parse_loyalty_discount line =
  let regexp = Str.regexp "loyalty_discount(\\(.*\\), \\(.*\\))." in
  if Str.string_match regexp line 0 then
    Some (Str.matched_group 1 line, float_of_string (Str.matched_group 2 line))
  else
    None

let parse_shipping_cost line =
  let regexp = Str.regexp "shipping_cost('\\(.*\\)', \\(.*\\))." in
  if Str.string_match regexp line 0 then
    Some (Str.matched_group 1 line, float_of_string (Str.matched_group 2 line))
  else
    None

let () =
  let file_lines = read_file "store.pl" in
  let discounts = List.filter_map parse_discount file_lines in
  let items = List.filter_map parse_item file_lines in
  let loyalty_discounts = List.filter_map parse_loyalty_discount file_lines in
  let shipping_costs = List.filter_map parse_shipping_cost file_lines in
  (* Print the parsed information *)
  List.iter (fun (category, discount) -> Printf.printf "Discount for category %s: %f\n" category discount) discounts;
  List.iter (fun (id, name, category, price, quantity) -> Printf.printf "Item: %d, %s, %s, %f, %d\n" id name category price quantity) items;
  List.iter (fun (years, discount) -> Printf.printf "Loyalty discount for %s year(s): %f\n" years discount) loyalty_discounts;
  List.iter (fun (district, cost) -> Printf.printf "Shipping cost to district %s: %f\n" district cost) shipping_costs


 (**********************************************************)
 (**********************************************************)
 (************** Shop Management functions *****************)
 (**********************************************************)
 (**********************************************************)

(* Function to convert the cart string into a list of items *)
let parse_cart_items cart_str =
  Str.split (Str.regexp ",") cart_str |> List.map (fun item_str ->
    match Str.split (Str.regexp ";") item_str with
    | [id; name; category; price; quantity] ->
        (int_of_string id, name, category, float_of_string price, int_of_string quantity)
    | _ -> failwith "Invalid item format")

(* Function to calculate the total price of the cart without discounts *)
let calculate_cart_total cart_items =
  List.fold_left (fun acc (_, _, _, price, quantity) ->
    acc +. (price *. float_of_int quantity)
  ) 0.0 cart_items

(* Function to calculate category discounts on cart items *)
let calculate_discounts_category cart_items =
  let file_lines = read_file "store.pl" in
  let discounts = List.filter_map parse_discount file_lines in
  List.fold_left (fun acc (_, _, category, price, quantity) ->
    let discount_rate = match List.find_opt (fun (cat, _) -> cat = category) discounts with
    | Some (_, rate) -> rate
    | None -> 0.0  (* No discount if the category is not found *)
    in
    let discount_amount = price *. discount_rate *. float_of_int quantity in
    acc +. discount_amount
  ) 0.0 cart_items

(* Function to calculate the loyalty discount based on the customer's years of loyalty *)
let calculate_loyalty_discount years total_amount =
  let file_lines = read_file "store.pl" in
  let loyalty_discounts = List.filter_map parse_loyalty_discount file_lines in
  let discount_rate =
    List.fold_left (fun acc (loyalty_years, rate) ->
      if loyalty_years = ">" ^ string_of_int (years - 1) || loyalty_years = "<" ^ string_of_int (years + 1) || loyalty_years = string_of_int years then
        max acc rate
      else
        acc
    ) 0.0 loyalty_discounts
  in
  total_amount *. discount_rate

(* Function to calculate the shipping cost based on the district *)
let calculate_shipping_cost district =
  let file_lines = read_file "store.pl" in
  let shipping_costs = List.filter_map parse_shipping_cost file_lines in
  match List.find_opt (fun (d, _) -> d = district) shipping_costs with
  | Some (_, cost) -> cost
  | None -> (* returns a default value or sends an error message *)
    Printf.printf "Warning: District '%s' not found, applying default shipping cost of 0.0\n" district;
    0.0

(* Function to calculate the final price of the cart *)
let calculate_final_cart_price cart_str years_of_loyalty district =
  let cart_items = parse_cart_items cart_str in
  let total_price = calculate_cart_total cart_items in
  let total_discount_category = calculate_discounts_category cart_items in
  let price_after_category_discounts = total_price -. total_discount_category in
  let loyalty_discount = calculate_loyalty_discount years_of_loyalty price_after_category_discounts in
  let price_after_all_discounts = price_after_category_discounts -. loyalty_discount in
  let shipping_cost = calculate_shipping_cost district in
  let final_price = price_after_all_discounts +. shipping_cost in
  max final_price 0.0

(* Function to compare two items first by category and then by item name *)
let compare_items (_, name1, category1, _, _) (_, name2, category2, _, _) =
  match String.compare category1 category2 with
  | 0 -> String.compare name1 name2  (* If the categories are the same, compare by name *)
  | c -> c  (* Otherwise, compare by category *)

(* Function to sort and display the shopping cart *)
let display_cart cart_str =
  let cart_items = parse_cart_items cart_str in
  let sorted_items = List.sort compare_items cart_items in
  List.iter (fun (_, name, category, price, quantity) ->
    Printf.printf "Sorted Item: %s, Category: %s, Price: %.2f, Quantity: %d\n" name category price quantity
  ) sorted_items

(* Execution *)
let main () =
  let args = Sys.argv in
  if Array.length args <> 4 then
    Printf.printf "Usage: %s cart_str years_of_loyalty district\n" args.(0)
  else
    let cart_str = args.(1) in
    let years_of_loyalty = int_of_string args.(2) in
    let district = args.(3) in

    let cart_items = parse_cart_items cart_str in
    let total_price = calculate_cart_total cart_items in
    Printf.printf "Total price of the cart without discounts: %f\n" total_price;

    let total_discount_category = calculate_discounts_category cart_items in
    Printf.printf "Total discount from categories: %f\n" total_discount_category;

    (* Subtract the category discount from the total price without discount *)
    let total_amount_after_category_discounts = total_price -. total_discount_category in
    let loyalty_discount = calculate_loyalty_discount (years_of_loyalty) total_amount_after_category_discounts in
    Printf.printf "Loyalty discount for years of loyalty: %f\n"  loyalty_discount;

    let shipping_cost = calculate_shipping_cost district in
    Printf.printf "Shipping cost: %f\n" shipping_cost; 

    let final_price = calculate_final_cart_price cart_str (years_of_loyalty) district in
    Printf.printf "The final price of the cart is: %f\n" final_price;

    display_cart cart_str
  
let () = main ()
