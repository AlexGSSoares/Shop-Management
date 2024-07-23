import java.io.*;
import java.util.*;
import org.jpl7.*;

/**
 * Represents the operational interface for a store that handles client transactions,
 * manages shopping carts, and records sales history using Prolog as a backend.
 */
public class Store {
    // Path to the Ocaml executable 
    private static String path = "/home/alexandre/efolioA/_build/default/bin/main.exe";
    // Path to the Prolog database
    private static final String prologFile = "store.pl";
    // Currently selected client for a session.
    private static Client selectedClient;
     // Date of the current purchase.
    private static String purchaseDate;
    // The shopping cart used during a purchase.
    private static Cart cart = new Cart();
    // Scanner for reading input from the console.
    private Scanner scanner;

    // Constructor that initializes a Store object with a scanner for input handling.
    public Store(Scanner scanner) {
        this.scanner = scanner;
    }

    //Executes the main loop for the store operations
    public void runStore() {
        int clientId = selectClient(); // Let the user select a client to operate on.
        if (clientId == -1) {
            System.out.println("No clients available or invalid selection.");
            return;
        }

        selectedClient = getClientById(clientId); // Retrieve details of the selected client.
        System.out.print("Enter the purchase date (dd/mm/yyyy): ");
        purchaseDate = scanner.nextLine().trim();

        int purchaseChoice;
        do {
            System.out.println("\nPurchase Menu:");
            System.out.println("1 - Shopping Cart");
            System.out.println("2 - Finalize Purchase");
            System.out.println("0 - Return to Main Menu");
            System.out.print("Enter your choice: ");
            purchaseChoice = scanner.nextInt();
            scanner.nextLine();  // Consume newline

            switch (purchaseChoice) {
                case 1:
                    shoppingCartMenu();
                    break;
                case 2:
                    finalizePurchase();
                    break;
                case 0:
                    System.out.println("Returning to Main Menu...");
                    break;
                default:
                    System.out.println("Invalid choice, please choose again.");
            }
        } while (purchaseChoice != 0);
    }

    //Displays a menu and handles the selection of a client from a list retrieved via Prolog.
    private int selectClient() {
        Query query = new Query("all_clients(Clients)");// Prolog query to fetch all clients.
        Map<String, Term>[] solutions = query.allSolutions();
        if (solutions == null || solutions.length == 0) {
            System.out.println("No clients found.");
            return -1;
        }

        Term clientsList = solutions[0].get("Clients");
        Term[] clients = clientsList.toTermArray();
        for (int i = 0; i < clients.length; i++) {
            Term[] clientDetails = clients[i].toTermArray();
            System.out.printf("%d - ID: %d, Name: %s, District: %s, Loyalty Years: %d\n",
                    (i + 1),
                    clientDetails[0].intValue(),
                    clientDetails[1].name(),
                    clientDetails[2].name(),
                    clientDetails[3].intValue());
        }

        System.out.print("Select a client by number: ");
        int choice = scanner.nextInt();
        scanner.nextLine(); // Consume newline
        // Return the client ID if selected properly, otherwise -1.
        if (choice < 1 || choice > clients.length) {
            return -1;
        }

        Term[] selectedClientDetails = clients[choice - 1].toTermArray();
        return selectedClientDetails[0].intValue();
    }

    //Retrieves a Client object by ID from the Prolog database.
    private static Client getClientById(int clientId) {
        // param clientId The ID of the client to retrieve.
        Query query = new Query("client(" + clientId + ", Name, District, LoyaltyYears)");
        if (query.hasSolution()) {
            Map<String, Term> solution = query.oneSolution();
            // return a new Client object if found, otherwise null.
            return new Client(
                    clientId,
                    solution.get("Name").name(),
                    solution.get("District").name(),
                    solution.get("LoyaltyYears").intValue()
            );
        }
        return null;
    }

    //Manages the shopping cart operations, such as adding items to the cart or viewing the cart.
    private void shoppingCartMenu() {
        int cartChoice;
        do {
            System.out.println("\nShopping Cart:");
            System.out.println("1 - Add Item to Cart");
            System.out.println("2 - See Shopping Cart");
            System.out.println("0 - Return to Purchase Menu");
            System.out.print("Enter your choice: ");
            cartChoice = scanner.nextInt();
            scanner.nextLine();  // Consume newline

            switch (cartChoice) {
                case 1:
                    addItemToCart();
                    break;
                case 2:
                    seeShoppingCart();
                    break;
                case 0:
                    return;
                default:
                    System.out.println("Invalid choice. Please try again.");
            }
        } while (cartChoice != 0);
    }

    // Adds an item to the shopping cart after presenting all available items.
    private void addItemToCart() {
        // Prolog query to fetch all items.
        Query query = new Query("all_items(Items)");
        Map<String, Term>[] solutions = query.allSolutions();
        if (solutions == null || solutions.length == 0) {
            System.out.println("No items found.");
            return;
        }
    
        Term itemsList = solutions[0].get("Items");
        Term[] items = itemsList.toTermArray();
        java.util.Map<java.lang.Integer, Item> itemMap = new java.util.HashMap<>();
    
        for (int i = 0; i < items.length; i++) {
            Term[] itemDetails = items[i].toTermArray();
            int id = itemDetails[0].intValue();
            String name = itemDetails[1].name();
            String category = itemDetails[2].name();
            double price = itemDetails[3].doubleValue();
    
            System.out.printf("%d - ID: %d, Name: %s, Category: %s, Price: %.2f\n",
                    (i + 1),
                    id,
                    name,
                    category,
                    price);
    
            itemMap.put(id, new Item(id, name, category, price, 0)); // Inicially, quantity = 0
        }
    
        System.out.print("Enter the item ID to add to cart: ");
        int itemId = scanner.nextInt();
        System.out.print("Enter the quantity: ");
        int quantity = scanner.nextInt();
        scanner.nextLine();  // Consume newline
    
        if (itemMap.containsKey(itemId)) {
            Item item = itemMap.get(itemId);
            cart.addItem(new Item(item.getId(), item.getName(), item.getCategory(), item.getPrice(), quantity));
            System.out.println("Item added to cart.");
        } else {
            System.out.println("Invalid item ID.");
        }
    }

    //Displays the contents of the shopping cart and calculates various totals and discounts.
    private static void seeShoppingCart() {
        CartOperations cartOperations = new CartOperations(path, selectedClient.getDistrict(), selectedClient.getYearsOfLoyalty(), cart.generateCartString());
        cartOperations.executeCartOperations();
        List<String> results = cartOperations.getResultLines();

        System.out.println("\nSorted Cart Items:");
        printAllResultsByKey(results, "Sorted Item");

        System.out.println("\nTotal price of the cart without discounts:");
        System.out.println(findResultByKeyword(results, "Total price of the cart without discounts:"));

        System.out.println("\nTotal discount from categories:");
        System.out.println(findResultByKeyword(results, "Total discount from categories:"));

        System.out.println("\nLoyalty discount for years of loyalty:");
        System.out.println(findResultByKeyword(results, "Loyalty discount for years of loyalty:"));

        System.out.println("\nShipping cost:");
        System.out.println(findResultByKeyword(results, "Shipping cost:"));

        System.out.println("\nThe final price of the cart is:");
        System.out.println(findResultByKeyword(results, "The final price of the cart is:"));
    }

    // Finalizes the purchase by processing discounts, updating inventory, and recording the transaction.
    private static void finalizePurchase() {
        CartOperations cartOperations = new CartOperations(path, selectedClient.getDistrict(), selectedClient.getYearsOfLoyalty(), cart.generateCartString());
        cartOperations.executeCartOperations();
        List<String> results = cartOperations.getResultLines();

        double valorSemDesconto = Double.parseDouble(findResultByKeyword(results, "Total price of the cart without discounts:"));
        double descontoCategoria = Double.parseDouble(findResultByKeyword(results, "Total discount from categories:"));
        double descontoLealdade = Double.parseDouble(findResultByKeyword(results, "Loyalty discount for years of loyalty:"));
        double custoEnvio = Double.parseDouble(findResultByKeyword(results, "Shipping cost:"));
        double total = Double.parseDouble(findResultByKeyword(results, "The final price of the cart is:"));

        if (updatePrologFile("add_purchase", selectedClient.getId(), purchaseDate, valorSemDesconto, descontoCategoria, descontoLealdade, custoEnvio, total)) {
            for (Item item : cart.getItems()) {
                updatePrologFile("update_inventory", item.getId(), item.getQuantity());
            }
            System.out.println("Purchase finalized.");
        } else {
            System.out.println("Failed to finalize purchase.");
        }
    }

    /**
     * Updates the Prolog file with transaction records or inventory updates.
     * action Specifies whether adding a purchase or updating inventory.
     * args The parameters required for updating, varying based on action.
     * return true if the update was successful, false otherwise.
     */
    private static boolean updatePrologFile(String action, Object... args) {
        List<String> fileContent = new ArrayList<>();
        boolean updateMade = false;
        int lastHistoryIndex = -1;

        try (BufferedReader br = new BufferedReader(new FileReader(prologFile))) {
            String line;
            int lineIndex = 0;
            while ((line = br.readLine()) != null) {
                if (line.startsWith("history_purchase(")) {
                    lastHistoryIndex = lineIndex;
                }
                if (action.equals("update_inventory") && line.startsWith("item(" + args[0] + ",")) {
                    String[] parts = line.split(", ");
                    int currentQuantity = java.lang.Integer.parseInt(parts[4].replaceAll("\\D", ""));
                    int quantitySold = (int) args[1];
                    int newQuantity = currentQuantity - quantitySold;
                    line = line.replaceFirst(", " + currentQuantity + "\\).", ", " + newQuantity + ").");
                    updateMade = true;
                }
                fileContent.add(line);
                lineIndex++;
            }
            if (action.equals("add_purchase")) {
                String newPurchase = String.format("history_purchase(%d, '%s', %.2f, %.2f, %.2f, %.2f, %.2f).",
                        (int) args[0], (String) args[1], (double) args[2], (double) args[3], (double) args[4], (double) args[5], (double) args[6]);
                if (lastHistoryIndex != -1) {
                    fileContent.add(lastHistoryIndex + 1, newPurchase);
                } else {
                    fileContent.add(newPurchase);
                }
                updateMade = true;
            }
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
        try (BufferedWriter bw = new BufferedWriter(new FileWriter(prologFile))) {
            for (String line : fileContent) {
                bw.write(line);
                bw.newLine();
            }
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
        return updateMade;
    }

    /**
     * Searches a list of strings for a line starting with the specified keyword and returns the result.
     * results = The list of strings to search through.
     * kkeyword = The keyword to search for at the beginning of the lines.
     * returns The line from the list that starts with the keyword, or a default message if not found.
     */
    private static String findResultByKeyword(List<String> results, String keyword) {
        return results.stream()
            .filter(line -> line.startsWith(keyword))
            .findFirst()
            .map(line -> line.substring(keyword.length() + 1))
            .orElse("Informação não encontrada para: " + keyword);
    }

    /**
     * Prints all lines from a list that start with the specified keyword.
     * results = The list of results to search through.
     * keyword = The keyword to filter by.
     */
    private static void printAllResultsByKey(List<String> results, String keyword) {
        results.stream()
            .filter(line -> line.startsWith(keyword))
            .forEach(System.out::println);  // Isso imprimirá cada linha que começa com a palavra-chave.
    }
}
