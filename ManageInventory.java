import org.jpl7.*;
import java.util.*;
import java.io.*;
public class ManageInventory {
    // Path to the Prolog file that contains the inventory and category data.
    private static final String prologFile = "store.pl";
    // Scanner object for reading input from the console.
    private Scanner scanner;

    // Constructor to initialize the scanner object.
    public ManageInventory(Scanner scanner) {
        this.scanner = scanner;
    }

    // Main method to run the inventory management interface.
    public void runInventoryManager() {
        // Consult the Prolog file to load data and rules.
        Query q1 = new Query("consult", new Term[]{new Atom(prologFile)});
        System.out.println("Consult " + (q1.hasSolution() ? "succeeded" : "FAILED"));
        int choice = 0;
        // Display a menu for the user to select an action.
        do {
            System.out.println("\nChoose an Option:");
            System.out.println("1 - View All Items");
            System.out.println("2 - View Items by Category");
            System.out.println("3 - View Available Categories");
            System.out.println("4 - Manage Categories");
            System.out.println("5 - Manage Item");
            System.out.println("0 - Exit");
            System.out.print("Enter your choice: ");
            choice = scanner.nextInt();
            scanner.nextLine(); // Consume newline left-over

            // Process the user's choice.
            switch (choice) {
                case 1:
                    displayAllItems();
                    break;
                case 2:
                    System.out.print("Enter category to filter by: ");
                    String inputCategory = scanner.nextLine().trim();
                    displayItemsByCategory(inputCategory);
                    break;
                case 3:
                    displayCategories();
                    break;
                case 4:
                    manageCategories();
                    break;
                case 5:
                    manageItems();
                    break;
                case 0:
                    System.out.println("Exiting...");
                    break;
                default:
                    System.out.println("Invalid choice, please choose again.");
            }
        } while (choice != 0);
        scanner.close(); // Closes the scanner object to free resources.
    }
    
    // Method to handle item management, including add, modify, and remove operations.
    private void manageItems() {
        int itemChoice;
        do {
            System.out.println("\nManage Items:");
            System.out.println("1 - Add a new item");
            System.out.println("2 - Modify an existing item");
            System.out.println("3 - Remove an item");
            System.out.println("0 - Return to main menu");
            System.out.print("Enter your choice: ");
            itemChoice = scanner.nextInt();
            scanner.nextLine(); // Consume newline left-over
    
            switch (itemChoice) {
                case 1:
                    addItem();
                    break;
                case 2:
                    modifyItem();
                    break;
                case 3:
                    removeItem();
                    break;
                case 0:
                    break;
                default:
                    System.out.println("Invalid choice, please choose again.");
            }
        } while (itemChoice != 0);
    }

    // Adds a new item to the inventory after validating ID uniqueness and category existence.
    private void addItem() {
        System.out.print("Enter item ID: ");
        int id = scanner.nextInt(); scanner.nextLine(); // consume newline
        if (itemExists(id)) {
            System.out.println("An item with this ID already exists. Please use a different ID.");
            return; // Return early if the item ID already exists.
        }
        System.out.print("Enter item name: ");
        String name = scanner.nextLine().trim();
        System.out.print("Enter item category: ");
        String category = scanner.nextLine().trim();
        if (!categoryExists(category)) {
            System.out.println("Category does not exist. Please add the category first.");
            return; // Return early if the category does not exist.
        }
        System.out.print("Enter item cost: ");
        double cost = scanner.nextDouble(); scanner.nextLine(); // consume newline
        System.out.print("Enter inventory quantity: ");
        int quantity = scanner.nextInt(); scanner.nextLine(); // consume newline
    
        // Add the item using the updateItem method and report success or failure.
        if (updateItem("add_item", id, name, category, cost, quantity)) {
            System.out.println("Item added successfully.");
        } else {
            System.out.println("Failed to add item.");
        }
    }
    
    // Modifies an existing item after validating category existence.
    private void modifyItem() {
        System.out.print("Enter item ID to modify: ");
        int id = scanner.nextInt(); scanner.nextLine();
        System.out.print("Enter new item name: ");
        String name = scanner.nextLine().trim();
        System.out.print("Enter new item category: ");
        String category = scanner.nextLine().trim();
         if (!categoryExists(category)) {
        System.out.println("Category does not exist. Please add the category first.");
        return; // Return early if the category does not exist.
    }
        System.out.print("Enter new item cost: ");
        double cost = scanner.nextDouble(); scanner.nextLine();
        System.out.print("Enter new inventory quantity: ");
        int quantity = scanner.nextInt(); scanner.nextLine();
    
        // Modify the item using the updateItem method and report success or failure.
        if (updateItem("modify_item", id, name, category, cost, quantity)) {
            System.out.println("Item modified successfully.");
        } else {
            System.out.println("Failed to modify item.");
        }
    }
    
     // Removes an item from the inventory.
    private void removeItem() {
        System.out.print("Enter item ID to remove: ");
        int id = scanner.nextInt(); scanner.nextLine();
    
        // Remove the item using the updateItem method and report success or failure.
        if (updateItem("remove_item", id, "", "", 0.0, 0)) {
            System.out.println("Item removed successfully.");
        } else {
            System.out.println("Failed to remove item.");
        }
    }
    
    // Handles category management, including add, modify, and remove operations.
    private void manageCategories() {
        int categoryChoice;
        do {
            System.out.println("\nManage Categories:");
            System.out.println("1 - Add a new category");
            System.out.println("2 - Modify an existing category");
            System.out.println("3 - Remove a category");
            System.out.println("0 - Return to main menu");
            System.out.print("Enter your choice: ");
            categoryChoice = scanner.nextInt();
            scanner.nextLine(); // Consume newline left-over

            switch (categoryChoice) {
                case 1:
                    addCategory();
                    break;
                case 2:
                    modifyCategory();
                    break;
                case 3:
                    removeCategory();
                    break;
                case 0:
                    break;
                default:
                    System.out.println("Invalid choice, please choose again.");
            }
        } while (categoryChoice != 0);
    }

    // Adds a new category with a specified discount after checking for uniqueness.
    private void addCategory() {
        System.out.print("Enter new category name: ");
        String categoryName = scanner.nextLine().trim();
        System.out.print("Enter discount for new category: ");
        String discountInput = scanner.nextLine().trim();
        double discount = Double.parseDouble(discountInput.replace(',', '.'));

        // Add the category using the updateCategory method and report success or failure.
        if (updateCategory("add", categoryName, discount)) {
            System.out.println("Category added successfully.");
        } else {
            System.out.println("Failed to add category.");
        }
    }
    
     // Modifies an existing category with a new discount value
    private void modifyCategory() {
        System.out.print("Enter category name to modify: ");
        String categoryName = scanner.nextLine().trim();
        System.out.print("Enter new discount for the category: ");
        String newDiscountInput = scanner.nextLine().trim();
        double newDiscount = Double.parseDouble(newDiscountInput.replace(',', '.'));
    
        // Modify the category using the updateCategory method and report success or failure.
        if (updateCategory("modify", categoryName, newDiscount)) {
            System.out.println("Category modified successfully.");
        } else {
            System.out.println("Failed to modify category.");
        }
    }
    
    // Removes a category from the system.
    private void removeCategory() {
        System.out.print("Enter category name to remove: ");
        String categoryName = scanner.nextLine().trim();
    
        // Remove the category using the updateCategory method and report success or failure.
        if (updateCategory("remove", categoryName, 0)) {
            System.out.println("Category removed successfully.");
        } else {
            System.out.println("Failed to remove category.");
        }
    }
    
    // Method to update item details in the inventory. Handles add, modify, and remove actions.
    private static boolean updateItem(String action, int id, String name, String category, double cost, int quantity) {
        List<String> fileContent = new ArrayList<>();
        boolean updateMade = false;
        boolean itemSectionFound = false;
        int lastItemIndex = -1;
    
        try (BufferedReader br = new BufferedReader(new FileReader(prologFile))) {
            String line;
            while ((line = br.readLine()) != null) {
                // Updates the index of the last item whenever it finds an item row
                if (line.startsWith("item(")) {
                    itemSectionFound = true;
                    lastItemIndex = fileContent.size(); // Must point to the current position before adding the line
                }
                // Checks if it is the correct item to modify or remove
                if (line.startsWith("item(" + id + ",")) {
                    System.out.println("Encontrado item com ID " + id); // Debug
                    if (action.equals("remove_item")) {
                        updateMade = true;
                        continue; // Skip adding this line to remove the item.
                    } else if (action.equals("modify_item")) {
                        // Replaces the line with the new content of the item
                        line = String.format("item(%d, '%s', '%s', %.2f, %d).", id, name, category, cost, quantity);
                        updateMade = true;
                    }
                }
                fileContent.add(line);
            }
            // Add a new item if necessary and not already done    
            if (action.equals("add_item") && !updateMade) {
                String newItem = String.format("item(%d, '%s', '%s', %.2f, %d).", id, name, category, cost, quantity);
                if (itemSectionFound && lastItemIndex != -1) {
                    fileContent.add(lastItemIndex + 1, newItem); // Insert the new item just after the last found item.
                } else {
                    fileContent.add(newItem);// Add at the end if no other items are listed.
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
    
    // Method to update category details in the system. Handles add, modify, and remove actions.
    private static boolean updateCategory(String action, String category, double discount) {
        List<String> fileContent = new ArrayList<>();
        boolean updateMade = false;
        boolean discountSectionFound = false;
        int lastDiscountIndex = -1; // To track where to add new discounts
    
        try (BufferedReader br = new BufferedReader(new FileReader(prologFile))) {
            String line;
            while ((line = br.readLine()) != null) {
                fileContent.add(line);
                if (line.startsWith("discount(")) {
                    discountSectionFound = true;
                    lastDiscountIndex = fileContent.size() - 1; // Última posição de desconto
                    if (line.contains("discount('" + category + "'")) {
                        if (action.equals("remove")) {
                            // Remove the last added discount line
                            fileContent.remove(lastDiscountIndex);
                            updateMade = true;
                            continue;
                        } else if (action.equals("modify")) {
                            // Modifies line
                            fileContent.set(lastDiscountIndex, "discount('" + category + "', " + discount + ").");
                            updateMade = true;
                        }
                    }
                }
            }
            if (action.equals("add") && !updateMade && discountSectionFound) {
                // Add a new discount category after the last discount
                fileContent.add(lastDiscountIndex + 1, "discount('" + category + "', " + discount + ").");
                updateMade = true;
            }
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
        // Write the updated content to the Prolog file
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
 * Processes a Prolog query that retrieves item data, constructing a list of Item objects from the results.
 * @param query The Prolog query object already configured to fetch item details.
 */
private static void processItemsQuery(Query query) {
    ArrayList<Item> items = new ArrayList<>(); // Initialize a list to hold the parsed items.
    Map<String, Term>[] solutions = query.allSolutions(); // Execute the query and store the solutions.

    // Check if the query returned any solutions.
    if (solutions != null) {
        // Iterate through each solution mapping found in the Prolog results.
        for (Map<String, Term> solution : solutions) {
            Term itemList = solution.get("Items"); // Extract the list of items from the current solution.

            // Process each item term in the retrieved list of items.
            for (Term item : itemList.toTermArray()) {
                Term[] details = item.toTermArray(); // Break down the item term into its constituent properties.

                // Verify that each item property is of the expected data type.
                if (!(details[0] instanceof org.jpl7.Integer &&
                      details[1] instanceof org.jpl7.Atom &&
                      details[2] instanceof org.jpl7.Atom &&
                      details[3] instanceof org.jpl7.Float &&
                      details[4] instanceof org.jpl7.Integer)) {
                    System.out.println("Unexpected data types in Prolog response."); // Log error if data types mismatch.
                    continue; // Skip to the next item if data types do not match.
                }

                // Create a new Item object using the parsed and validated data.
                Item newItem = new Item(
                    details[0].intValue(),     // Convert Term to integer for ID.
                    details[1].name(),         // Convert Atom to String for name.
                    details[2].name(),         // Convert Atom to String for category.
                    details[3].doubleValue(),  // Convert Float to double for cost.
                    details[4].intValue()      // Convert Integer to int for quantity.
                );

                items.add(newItem); // Add the new item to the list of items.
            }
        }
    }

    // After processing all solutions, check if any items were found and added to the list.
    if (items.isEmpty()) {
        System.out.println("No items found."); // Notify if no items were parsed.
    } else {
        // If items were found, print each item using its toString method.
        for (Item item : items) {
            System.out.println(item);
        }
    }
}

    // Checks if a category exists by querying the Prolog database.
    private static boolean categoryExists(String category) {
        Query categoryQuery = new Query("discount", new Term[] {new Atom(category), new Variable("Discount")});
        return categoryQuery.hasSolution();
    }

    // Checks if an item exists by querying the Prolog database.
    private static boolean itemExists(int id) {
        Query itemQuery = new Query("item", new Term[] {new org.jpl7.Integer(id), new Variable("Name"), new Variable("Category"), new Variable("Cost"), new Variable("Quantity")});
        return itemQuery.hasSolution();
    }    

    // Displays all items in the inventory using a Prolog query.
    private static void displayAllItems() {
        Query q2 = new Query("all_items(Items)");
        processItemsQuery(q2);
    }
    
    // Displays items filtered by category using a Prolog query.
    private static void displayItemsByCategory(String category) {
        Query q2 = new Query("items_by_category", new Term[] {new Atom(category), new Variable("Items")});
        processItemsQuery(q2);
    }

    // Displays available categories using a Prolog query.
    private static void displayCategories() {
        Query q2 = new Query("available_categories(Categories)");
        Map<String, Term>[] results = q2.allSolutions();
        if (results != null && results.length > 0) {
            System.out.println("Available categories:");
            for (Term category : results[0].get("Categories").toTermArray()) {
                System.out.println("- " + category.name());
            }
        } else {
            System.out.println("No categories found.");
        }
    }   
}
