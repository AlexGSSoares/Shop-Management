/**********************************************************
 ***************          EFOLIO B         ****************
 **********      Linguagens de Programação      ***********
 **********************************************************
 ***********    Professor: RICARDO BAPTISTA     ***********
 ***********   Monitor: RÚDI GUALTER OLIVEIRA   ***********
 ************     Aluno: Alexandre Soares      ************
 *************      Nr de Aluno: 2101521      *************
 *********************************************************/

% Declaration Dynamic facts
:- dynamic item/5.
:- dynamic discount/2.
:- dynamic loyalty_discount/2.
:- dynamic shipping_cost/2.
:- dynamic client/4.
:- dynamic history_purchase/7.

% Item em inventário
/*id_item, nome, categoria, custo, quantidade_inventário*/
item(1, 'Potion of Healing', 'potions', 10.0, 50).
item(2, 'Wand of Fireball', 'wands', 20.0, 30).
item(3, 'Enchanted Spellbook', 'enchanted_books', 30.0, 20).
item(4, 'Crystal of Clairvoyance', 'crystals', 15.0, 40).
item(5, 'Amulet of Protection', 'amulets', 25.0, 25).
item(6, 'Standard Wand', 'wands', 20.0, 100).
item(7, 'Love Potion', 'potions', 10.0, 50).
item(8, 'Advanced Spellbook', 'enchanted_books', 15.0, 30).
item(9, 'Crystal of Magic Vision', 'crystals', 30.0, 20).
item(10, 'Flying Broomstick', 'accessories', 50.0, 10).
item(11, 'Enchanted Scroll', 'scrolls', 8.0, 50).
item(12, 'Fairy Dust', 'ingredients', 5.0, 100).



% Descontos por categoria de item
/*categoria, desconto*/
discount('potions', 0.1).
discount('wands', 0.2).
discount('enchanted_books', 0.3).
discount('crystals', 0.15).
discount('amulets', 0.25).
discount('accessories', 0.0).
discount('scrolls', 0.2).
discount('ingredients', 0.05).

% Desconto de lealdade por ano
/*ano_lealdade, desconto*/
loyalty_discount(1, 0.05).
loyalty_discount(2, 0.1).
loyalty_discount(3, 0.15).
loyalty_discount(4, 0.2).
loyalty_discount(5, 0.25).
loyalty_discount(years, 0.30) :- years >5.


% Custos de envio por distrito
/*Distrito, custo*/
shipping_cost('Aveiro', 5.00).
shipping_cost('Lisboa', 7.0).
shipping_cost('Porto', 11.00).
shipping_cost('Braga', 2.5).
shipping_cost('Coimbra', 5.0).
shipping_cost('Faro', 15.0).
shipping_cost('Viseu', 3.0).


% Clientes
/*id, nome, Distrito, anos_lealdade*/
client(1, 'Alice', 'Aveiro', 3).
client(2, 'Beatriz', 'Braga', 1).
client(3, 'Carlos', 'Coimbra', 2).
client(4, 'Diogo', 'Lisboa', 4).
client(5, 'Eva', 'Porto', 1).
client(6, 'Francisca', 'Faro', 3).
client(7, 'Guilhermina', 'Viseu', 5).

% Histórico de Compras
/* id_cliente, data, valor_sem_desconto, desconto_categoria, desconto_lealdade, custo_envio, total */
history_purchase(1, '20/05/2024', 50, 5, 0, 5, 50).
history_purchase(2, '21/05/2024', 30, 3, 1, 3, 29).
history_purchase(3, '22/05/2024', 40, 4, 0, 4, 40).
history_purchase(4, '23/05/2024', 60, 6, 2.5, 6, 57.5).
history_purchase(5, '23/05/2024', 25, 2.5, 0, 2.5, 25).
history_purchase(6, '25/05/2024', 35, 3.5, 2, 3.5, 33).
history_purchase(7, '26/05/2024', 75, 7.5, 0, 7.5, 75).
history_purchase(3, '27/05/2024', 45, 4.5, 0, 4.5, 45).
history_purchase(4, '28/05/2024', 55, 5.5, 10, 5, 44.5).
history_purchase(1, '28/05/2024', 60, 6, 0, 6, 60).




/*
*********************************************************
********** Predicados ficheiro ManageInventory **********
*********************************************************
*/
% Predicados para visualizar itens e categorias
all_items(Items) :-
    findall([ID, Name, Category, Cost, Quantity], item(ID, Name, Category, Cost, Quantity), Items).

% Items por categoria
items_by_category(Category, Items) :-
    findall([ID, Name, Category, Cost, Quantity], item(ID, Name, Category, Cost, Quantity), Items).

% Predicado para listar todas as categorias disponíveis com base em descontos definidos
available_categories(Categories) :-
    setof(Category, Discount^discount(Category, Discount), Categories).

% Adicionar uma nova categoria de desconto
add_discount(Category, Discount) :-
    \+ discount(Category, _),
    assertz(discount(Category, Discount)).

% Modificar um desconto existente
modify_discount(Category, NewDiscount) :-
    retract(discount(Category, _)),
    assertz(discount(Category, NewDiscount)).

% Remover uma categoria de desconto
remove_discount(Category) :-
    retract(discount(Category, _)).

% Adicionar um novo item
add_item(Id, Name, Category, Cost, Quantity) :-
    \+ item(Id, _, _, _, _),
    assertz(item(Id, Name, Category, Cost, Quantity)).

% Modificar um item existente
modify_item(Id, Name, Category, Cost, Quantity) :-
    retract(item(Id, _, _, _, _)),
    assertz(item(Id, Name, Category, Cost, Quantity)).

% Remover um item
remove_item(Id) :-
    retract(item(Id, _, _, _, _)).

/*
*********************************************************
*********** Predicados ficheiro ManageClients ***********
*********************************************************
*/

% Listar todos os clientes
all_clients(Clients) :-
    findall([ID, Name, District, LoyaltyYears], client(ID, Name, District, LoyaltyYears), Clients).

% Listar clientes por distrito
clients_by_district(District, Clients) :-
    findall([ID, Name, District, LoyaltyYears], client(ID, Name, District, LoyaltyYears), Clients).

client_purchases(ClientID, Purchases) :-
    findall([Date, PreDiscountAmount, CategoryDiscount, LoyaltyDiscount, ShippingCost, Total], 
            history_purchase(ClientID, Date, PreDiscountAmount, CategoryDiscount, LoyaltyDiscount, ShippingCost, Total),
            Purchases).

% Listar clientes com lealdade acima de um certo valor
clients_by_loyalty(MinLoyalty, Clients) :-
    findall([ID, Name, District, LoyaltyYears], (client(ID, Name, District, LoyaltyYears), LoyaltyYears > MinLoyalty), Clients).

% Adicionar um novo cliente
add_client(ID, Name, District, LoyaltyYears) :-
    \+ client(ID, _, _, _),
    assertz(client(ID, Name, District, LoyaltyYears)).

% Modificar um cliente existente
modify_client(ID, NewName, NewDistrict, NewLoyaltyYears) :-
    retract(client(ID, _, _, _)),
    assertz(client(ID, NewName, NewDistrict, NewLoyaltyYears)).

% Remover um cliente
remove_client(ID) :-
    retract(client(ID, _, _, _)).

/*
*********************************************************
****** Predicados ficheiro ManageCostsandDiscounts ******
*********************************************************
*/

% Listar todos os custos de envio
all_shipping_costs(Costs) :-
    findall([District, Cost], shipping_cost(District, Cost), Costs).

% Adicionar um novo custo de envio
add_shipping_cost(District, Cost) :-
    \+ shipping_cost(District, _),
    assertz(shipping_cost(District, Cost)).

% Modificar um custo de envio existente
modify_shipping_cost(District, NewCost) :-
    retract(shipping_cost(District, _)),
    assertz(shipping_cost(District, NewCost)).

% Remover um custo de envio
remove_shipping_cost(District) :-
    retract(shipping_cost(District, _)).

% Listar todos os descontos de categoria
all_category_discounts(Discounts) :-
    findall([Category, Discount], discount(Category, Discount), Discounts).

% Lista dos 5 primeiros descontos de lealdade + valor aritmético
all_loyalty_discounts(Discounts) :-
     findall([Year, Discount], (between(1, 5, Year), loyalty_discount(Year, Discount)), TempDiscounts),
     append(TempDiscounts, [['>5', 0.30]], Discounts).

% Adicionar um desconto de lealdade
add_loyalty_discount(Years, Discount) :-
    \+ loyalty_discount(Years, _),
    assertz(loyalty_discount(Years, Discount)).

% Modificar um desconto de lealdade existente
modify_loyalty_discount(Years, NewDiscount) :-
    retract(loyalty_discount(Years, _)),
    assertz(loyalty_discount(Years, NewDiscount)).

% Remover um desconto de lealdade
remove_loyalty_discount(Years) :-
    retract(loyalty_discount(Years, _)).

/*
*********************************************************
************ Predicados ficheiro SalesHistory ***********
*********************************************************
*/

% Ver todas as vendas de uma determinada data:
sales_by_date(Date, Sales) :-
    findall([ClientID, Date, PreDiscountAmount, CategoryDiscount, LoyaltyDiscount, ShippingCost, Total],
            history_purchase(ClientID, Date, PreDiscountAmount, CategoryDiscount, LoyaltyDiscount, ShippingCost, Total),
            Sales).


% Ver todas as vendas a um determinado cliente:
sales_by_client(ClientID, Sales) :-
    findall([ClientID, Date, PreDiscountAmount, CategoryDiscount, LoyaltyDiscount, ShippingCost, Total],
            history_purchase(ClientID, Date, PreDiscountAmount, CategoryDiscount, LoyaltyDiscount, ShippingCost, Total),
            Sales).

% Ver os totais das vendas de um determinado distrito:
sales_by_district(District, Sales) :-
    findall([ClientID, Date, PreDiscountAmount, CategoryDiscount, LoyaltyDiscount, ShippingCost, Total],
            (client(ClientID, _, District, _), history_purchase(ClientID, Date, PreDiscountAmount, CategoryDiscount, LoyaltyDiscount, ShippingCost, Total)),
            Sales).

% Ver os totais das vendas de um determinado distrito:
district_sales_totals(District, Totals) :-
    findall([PreDiscountAmount, CategoryDiscount, LoyaltyDiscount, ShippingCost, Total],
            (client(ClientID, _, District, _), history_purchase(ClientID, _, PreDiscountAmount, CategoryDiscount, LoyaltyDiscount, ShippingCost, Total)),
            Sales),
    foldl(sum_sales, Sales, [0,0,0,0,0], Totals).

sum_sales([A,B,C,D,E], [F,G,H,I,J], [AF,BG,CH,DI,EJ]) :-
    AF is A+F, BG is B+G, CH is C+H, DI is D+I, EJ is E+J.

% Ver os totais das vendas de uma determinada data:
date_sales_totals(Date, Totals) :-
    findall([PreDiscountAmount, CategoryDiscount, LoyaltyDiscount, ShippingCost, Total],
            history_purchase(_, Date, PreDiscountAmount, CategoryDiscount, LoyaltyDiscount, ShippingCost, Total),
            Sales),
    foldl(sum_sales, Sales, [0,0,0,0,0], Totals).

% Calcula os totais de desconto (categoria + lealdade) por distrito
discounts_by_district(District, TotalDiscount) :-
    client(ClientID, _, District, _),
    findall(DiscountSum, 
            (history_purchase(ClientID, _, _, CategoryDiscount, LoyaltyDiscount, _, _),
             DiscountSum is CategoryDiscount + LoyaltyDiscount),
            Discounts),
    sum_list(Discounts, TotalDiscount).

% Encontra o distrito com mais descontos
district_most_discounts(District) :-
    findall([Dist, TotalDiscount], discounts_by_district(Dist, TotalDiscount), Discounts),
    max_member([District, _], Discounts).

/*
*********************************************************
**************** Predicados ficheiro Store **************
*********************************************************
*/

% Atualiza o inventário ao diminuir a quantidade comprada
update_inventory(ItemId, QuantitySold) :-
    item(ItemId, Name, Category, Cost, CurrentQuantity),
    NewQuantity is CurrentQuantity - QuantitySold,
    retract(item(ItemId, Name, Category, Cost, CurrentQuantity)),
    assertz(item(ItemId, Name, Category, Cost, NewQuantity)).

