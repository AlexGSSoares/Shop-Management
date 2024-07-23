import org.jpl7.*;
import java.util.*;
import java.io.*;

public class ManageCostsAndDiscounts {
    // Path to the Prolog file that contains the business logic for costs and discounts.
    private static final String prologFile = "store.pl";
    // Scanner object to read input from the console.
    private Scanner scanner;

    // Constructor initializing the scanner object.
    public ManageCostsAndDiscounts(Scanner scanner) {
        this.scanner = scanner;
    }

    // Main method to run the costs and discounts management.
    public void runCostsAndDiscountsManager() {
        // Consult the Prolog file to load data and rules.
        Query q1 = new Query("consult", new Term[]{new Atom(prologFile)});
        System.out.println("Consult " + (q1.hasSolution() ? "succeeded" : "FAILED"));
        int choice;

        // Display a menu for the user to select an action.
        do {
            System.out.println("\nChoose an Option:");
            System.out.println("1 - View All Shipping Costs");
            System.out.println("2 - View All Category Discounts");
            System.out.println("3 - View All Loyalty Discounts");
            System.out.println("4 - Manage Shipping Costs");
            System.out.println("5 - Manage Loyalty Discounts");
            System.out.println("0 - Exit");
            System.out.print("Enter your choice: ");
            choice = scanner.nextInt();
            scanner.nextLine(); // Consumes the newline character after the number input.

            // Process the user's choice.
            switch (choice) {
                case 1:
                    displayAllShippingCosts();
                    break;
                case 2:
                    displayAllCategoryDiscounts();
                    break;
                case 3:
                    displayAllLoyaltyDiscounts();
                    break;
                case 4:
                    manageShippingCosts();
                    break;
                case 5:
                    manageLoyaltyDiscounts();
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

    // Display all category discounts using a Prolog query.
    private static void displayAllCategoryDiscounts() {
        Query query = new Query("all_category_discounts(Discounts)");
        processDiscountsQuery(query, "Category Discount");
    }
    
    // Display all loyalty discounts using a Prolog query.
    private static void displayAllLoyaltyDiscounts() {
        Query query = new Query("all_loyalty_discounts(Discounts)");
        processDiscountsQuery(query, "Loyalty Discount");
    }

    // General method to process discount queries and print results.
    private static void processDiscountsQuery(Query query, String type) {
        try {
            Map<String, Term>[] solutions = query.allSolutions();
            if (solutions != null && solutions.length > 0) {
                System.out.println(type + "s:");
                for (Map<String, Term> solution : solutions) {
                    Term discountsList = solution.get("Discounts");
                    for (Term discount : discountsList.toTermArray()) {
                        Term[] details = discount.toTermArray();
                        System.out.println("Years: " + details[0] + ", Discount: " + details[1]);
                    }
                }
            } else {
                System.out.println("No " + type.toLowerCase() + "s found.");
            }
        } catch (Exception e) {
            System.out.println("Failed to retrieve " + type.toLowerCase() + "s: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    // Display all shipping costs using a Prolog query.
    private static void displayAllShippingCosts() {
        Query query = new Query("all_shipping_costs(Costs)");
        processCostsQuery(query, "Shipping Cost");
    }
    
    // General method to process cost queries and print results.
    private static void processCostsQuery(Query query, String type) {
        try {
            Map<String, Term>[] solutions = query.allSolutions();
            if (solutions != null && solutions.length > 0) {
                System.out.println(type + "s:");
                for (Map<String, Term> solution : solutions) {
                    Term costsList = solution.get("Costs");
                    for (Term cost : costsList.toTermArray()) {
                        Term[] details = cost.toTermArray();
                        System.out.println("District: " + details[0].name() + ", Cost: " + details[1].doubleValue());
                    }
                }
            } else {
                System.out.println("No " + type.toLowerCase() + "s found.");
            }
        } catch (Exception e) {
            System.out.println("Failed to retrieve " + type.toLowerCase() + "s: " + e.getMessage());
        }
    }

    //Methods to manage shipping costs
    private void manageShippingCosts() {
        int choice;
        do {
            // Interface to Add, modify and Remove 
            System.out.println("\nManage Shipping Costs:");
            System.out.println("1 - Add a new shipping cost");
            System.out.println("2 - Modify an existing shipping cost");
            System.out.println("3 - Remove a shipping cost");
            System.out.println("0 - Return to main menu");
            System.out.print("Enter your choice: ");
            choice = scanner.nextInt();
            scanner.nextLine();  // Consume newline left-over

            switch (choice) {
                case 1:
                    addShippingCost();
                    break;
                case 2:
                    modifyShippingCost();
                    break;
                case 3:
                    removeShippingCost();
                    break;
                case 0:
                    break;
                default:
                    System.out.println("Invalid choice, please choose again.");
            }
        } while (choice != 0);
    }

    //Methods to manage Loyalty Discounts
    private void manageLoyaltyDiscounts() {
        int choice;
        do {
            // Interface to Add, modify and Remove 
            System.out.println("\nManage Loyalty Discounts:");
            System.out.println("1 - Add a new loyalty discount");
            System.out.println("2 - Modify an existing loyalty discount");
            System.out.println("3 - Remove a loyalty discount");
            System.out.println("0 - Return to main menu");
            System.out.print("Enter your choice: ");
            choice = scanner.nextInt();
            scanner.nextLine();  // Consume newline left-over

            switch (choice) {
                case 1:
                    addLoyaltyDiscount();
                    break;
                case 2:
                    modifyLoyaltyDiscount();
                    break;
                case 3:
                    removeLoyaltyDiscount();
                    break;
                case 0:
                    break;
                default:
                    System.out.println("Invalid choice, please choose again.");
            }
        } while (choice != 0);
    }

    private void addShippingCost() {
        // Prompt the user to enter the name of the district for which to add shipping costs.
        System.out.print("Enter district: ");
        String district = scanner.nextLine().trim(); // Read and trim any leading or trailing whitespace from the input.
        
        // Prompt the user to enter the cost associated with shipping to the specified district.
        System.out.print("Enter shipping cost: ");
        String costInput = scanner.nextLine(); // Read the cost input as a string.
        double cost = parseDoubleInput(costInput); // Convert the input string to a double using a helper method.
        
        // Check if the parsed double is a valid number. If not, notify the user and exit the method.
        if (Double.isNaN(cost)) {
            System.out.println("Invalid shipping cost format.");
            return; // Exit the method because the input format is incorrect.
        }
        
        // Check if a shipping cost entry already exists for the given district.
        if (!shippingCostExist(district)) {
            // If no existing entry is found, attempt to add the new shipping cost to the Prolog database.
            if (updateData(prologFile, "add", "shipping_cost", district, cost)) {
                // If the update is successful, inform the user.
                System.out.println("Shipping cost added successfully.");
            } else {
                // If the update fails, inform the user.
                System.out.println("Failed to add shipping cost.");
            }
        } else {
            // If a shipping cost entry already exists for the district, inform the user.
            System.out.println("Shipping cost already exists for this district.");
        }
    }    

    // verificacion of shipping costs Exist
    private static boolean shippingCostExist(String district) {
        // create district with name Atom
        Term districtTerm = new Atom(district);
    
        // Creating the Prolog query to check if there are any costs associated with this district
        Query query = new Query("shipping_cost", new Term[]{districtTerm, new Variable("Cost")});
    
        // Returns true if there is a solution, which means that there is a shipping cost for this district
        return query.hasSolution();
    }
    
    private void modifyShippingCost() {
        // Prompt the user to enter the district for which the shipping cost will be modified.
        System.out.print("Enter district: ");
        String district = scanner.nextLine().trim();
    
        // Prompt the user to enter the new shipping cost.
        System.out.print("Enter new shipping cost: ");
        String costInput = scanner.nextLine();
        double newCost = parseDoubleInput(costInput); // Convert the string input to a double.
        
        // Validate the input cost; if invalid, notify the user and exit the function.
        if (Double.isNaN(newCost)) {
            System.out.println("Invalid shipping cost format.");
            return;
        }
        
        // Update the shipping cost in the data file if the input is valid.
        if (updateData(prologFile, "modify", "shipping_cost", district, newCost)) {
            System.out.println("Shipping cost modified successfully.");
        } else {
            System.out.println("Failed to modify shipping cost.");
        }
    }
    
    private void removeShippingCost() {
        // Prompt the user to specify the district from which to remove the shipping cost.
        System.out.print("Enter district to remove: ");
        String district = scanner.nextLine().trim();
        
        // Attempt to remove the shipping cost from the data file.
        if (updateData(prologFile, "remove", "shipping_cost", district, 0)) {
            System.out.println("Shipping cost removed successfully.");
        } else {
            System.out.println("Failed to remove shipping cost.");
        }
    }
    
    private void addLoyaltyDiscount() {
        // Prompt the user to enter the number of loyalty years for the discount.
        System.out.print("Enter loyalty years: ");
        int years = scanner.nextInt();
        
        // Prompt for the discount percentage associated with the loyalty years.
        System.out.print("Enter discount: ");
        scanner.nextLine(); // Clean up the input stream.
        String discountInput = scanner.nextLine();
        double discount = parseDoubleInput(discountInput); // Convert input to double.
        
        // Check if the discount format is valid; if not, terminate the function.
        if (Double.isNaN(discount)) {
            return;
        }
        // Check if a loyalty discount for the specified years already exists.
        if (!loyaltyDiscountExists(years)) {
            // If it doesn't exist, attempt to add the new discount.
            if (updateData(prologFile, "add", "loyalty_discount", String.valueOf(years), discount)) {
                System.out.println("Loyalty discount added successfully.");
            } else {
                System.out.println("Failed to add loyalty discount.");
            }
        } else {
            System.out.println("A loyalty discount for these years already exists.");
        }
    }

    private static boolean loyaltyDiscountExists(int years) {
        // Converte os anos em um objeto Integer do Prolog
        Term yearsTerm = new org.jpl7.Integer(years);
    
        // Cria a consulta Prolog verificando se existe um desconto associado a esses anos específicos
        Query query = new Query("loyalty_discount", new Term[]{yearsTerm, new Variable("Discount")});
    
        // Executa a consulta e verifica se alguma solução é encontrada
        return query.hasSolution();
    }
    
    private void modifyLoyaltyDiscount() {
        // Prompt the user to enter the number of loyalty years for modification.
        System.out.print("Enter loyalty years: ");
        int years = scanner.nextInt();
        
        // Prompt for the new discount percentage.
        System.out.print("Enter new discount: ");
        scanner.nextLine();
        String discountInput = scanner.nextLine();
        double newDiscount = parseDoubleInput(discountInput);
        
        // Validate the new discount format.
        if (Double.isNaN(newDiscount)) {
            System.out.println("Invalid discount format.");
            return;
        }
        
        // Attempt to modify the existing discount.
        if (updateData(prologFile, "modify", "loyalty_discount", String.valueOf(years), newDiscount)) {
            System.out.println("Loyalty discount modified successfully.");
        } else {
            System.out.println("Failed to modify loyalty discount.");
        }
    }
    
    private void removeLoyaltyDiscount() {
        // Prompt the user to specify the loyalty years to remove the discount.
        System.out.print("Enter loyalty years to remove: ");
        int years = scanner.nextInt(); scanner.nextLine();
        
        // Attempt to remove the loyalty discount from the data file.
        if (updateData(prologFile, "remove", "loyalty_discount", String.valueOf(years), 0)) {
            System.out.println("Loyalty discount removed successfully.");
        } else {
            System.out.println("Failed to remove loyalty discount.");
        }
    }
    
    private static double parseDoubleInput(String input) {
        // Try to parse the input string to a double, handling different locale formats.
        try {
            return Double.parseDouble(input.replace(',', '.')); // Replace commas with dots for international formats.
        } catch (NumberFormatException e) {
            System.out.println("Invalid discount format. Please use the correct decimal format.");
            return Double.NaN; // Return NaN to indicate an error in conversion.
        }
    }   

    private static boolean updateData(String filePath, String action, String type, String identifier, double value) {
        List<String> fileContent = new ArrayList<>();
        boolean updateMade = false;
        boolean sectionFound = false;
        int lastIndex = -1;
    
        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.startsWith(type + "(")) {
                    sectionFound = true;
                    lastIndex = fileContent.size(); // Update the last index where the section starts.
                }
                if (line.matches("^" + type + "\\('" + identifier + "'.*$") && (action.equals("modify") || action.equals("remove"))) {
                    if (action.equals("remove")) {
                        updateMade = true; // Indicate that an update occurred.
                        continue; // Skip adding this line to effectively remove it.
                    } else {
                        line = String.format(type + "('%s', %.2f).", identifier, value); // Modify the line with new values.
                        updateMade = true;
                    }
                }
                fileContent.add(line); // Add the line to the updated content.
            }
            if (action.equals("add") && !updateMade && sectionFound) {
                String newData = String.format(type + "('%s', %.2f).", identifier, value); // Format the new data for addition.
                fileContent.add(lastIndex + 1, newData); // Add the new data in the section.
                updateMade = true;
            } else if (action.equals("add") && !updateMade) {
                String newData = String.format(type + "('%s', %.2f).", identifier, value);
                fileContent.add(newData); // Add new data at the end if no section was found.
                updateMade = true;
            }
        } catch (IOException e) {
            e.printStackTrace();
            return false; // Return false if there was an I/O error.
        }
    
        try (BufferedWriter bw = new BufferedWriter(new FileWriter(filePath))) {
            for (String contentLine : fileContent) {
                bw.write(contentLine);
                bw.newLine(); // Ensure proper line breaks.
            }
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
    
        return updateMade; // Return true if the data was successfully updated.
    }    
}