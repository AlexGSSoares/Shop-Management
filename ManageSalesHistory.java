import org.jpl7.*;
import java.util.*;

public class ManageSalesHistory {
    // Path to the Prolog file 
    private static final String prologFile = "store.pl";
    // Scanner object for reading input from the console.
    private Scanner scanner;

    /*
      Constructor for initializing the ManageSalesHistory with 
      a Scanner object, used to read input from the console.
     */
    public ManageSalesHistory(Scanner scanner) {
        this.scanner = scanner;
    }
    //Displays a menu and handles user input to navigate different sales viewing options.
    public void displayMenu() {
         // Consult the Prolog database before performing any operations.
        Query q1 = new Query("consult", new Term[]{new Atom(prologFile)});
        System.out.println("Consult " + (q1.hasSolution() ? "succeeded" : "FAILED"));
        int choice = 0;

        do {
             // Display various options related to sales history.
            System.out.println("\nChoose an Option:");
            System.out.println("1 - View All Sales by Date");
            System.out.println("2 - View All Sales by Client");
            System.out.println("3 - View All Sales by District");
            System.out.println("4 - View Total Sales by District");
            System.out.println("5 - View Total Sales by Date");
            System.out.println("6 - District with Most Discounts");
            System.out.println("0 - Exit");
            System.out.print("Enter your choice: ");
            choice = scanner.nextInt();
            scanner.nextLine(); // Consume newline left-over

            // Process the user's choice and call the appropriate method.
            switch (choice) {
                case 1:
                    displaySalesByDate();
                    break;
                case 2:
                    displaySalesByClient();
                    break;
                case 3:
                    displaySalesByDistrict();
                    break;
                case 4:
                    displayTotalSalesByDistrict();
                    break;
                case 5:
                    displayTotalSalesByDate();
                    break;
                case 6:
                    displayDistrictWithMostDiscounts();
                    break;
                case 0:
                    System.out.println("Exiting...");
                    break;
                default:
                    System.out.println("Invalid choice, please choose again.");
            }
        } while (choice != 0);
        scanner.close();
    }

    // Displays sales by a specific date provided by the user.
    private void displaySalesByDate() {
        System.out.print("Enter date (dd/mm/yyyy): ");
        String date = scanner.nextLine().trim();
        Query q = new Query("sales_by_date", new Term[] {new Atom(date), new Variable("Sales")});
        List<Purchase> purchases = processSalesQuery(q);
        displayPurchases(purchases);
    }
    //  Displays sales by a specific client ID provided by the user.
    private void displaySalesByClient() {
        System.out.print("Enter client ID: ");
        int clientId = scanner.nextInt();
        scanner.nextLine(); // Consume newline
        Query q = new Query("sales_by_client", new Term[] {new org.jpl7.Integer(clientId), new Variable("Sales")});
        List<Purchase> purchases = processSalesQuery(q);
        displayPurchases(purchases);
    }

    //  Displays sales by a specific district provided by the user.
    private  void displaySalesByDistrict() {
        System.out.print("Enter district: ");
        String district = scanner.nextLine().trim();
        Query q = new Query("sales_by_district", new Term[] {new Atom(district), new Variable("Sales")});
        List<Purchase> purchases = processSalesQuery(q);
        displayPurchases(purchases);
    }

    //Displays total sales by a specific district provided by the user.
    private void displayTotalSalesByDistrict() {
        System.out.print("Enter district: ");
        String district = scanner.nextLine().trim();
        Query q = new Query("district_sales_totals", new Term[] {new Atom(district), new Variable("Totals")});
        processTotalsQuery(q);
    }

    // Displays total sales by a specific date provided by the user.
    private void displayTotalSalesByDate() {
        System.out.print("Enter date (dd/mm/yyyy): ");
        String date = scanner.nextLine().trim();
        Query q = new Query("date_sales_totals", new Term[] {new Atom(date), new Variable("Totals")});
        processTotalsQuery(q);
    }

    // Displays the district with the most discounts.
    private static void displayDistrictWithMostDiscounts() {
        Query q = new Query("district_most_discounts", new Term[] {new Variable("District")});
        if (q.hasSolution()) {
            Map<String, Term> solution = q.oneSolution();
            System.out.println("District with the most discounts: " + solution.get("District").toString());
        } else {
            System.out.println("No district found.");
        }
    }

     /**
     * Processes a query to extract sales data and converts them into Purchase objects.
     * The Prolog query object configured to fetch sales data.
     * Returns A list of Purchase objects representing individual sales.
     */
    private static List<Purchase> processSalesQuery(Query query) {
        List<Purchase> purchases = new ArrayList<>();
        Map<String, Term>[] solutions = query.allSolutions();
        if (solutions != null && solutions.length > 0) {
            for (Map<String, Term> solution : solutions) {
                Term salesList = solution.get("Sales");
                for (Term sale : salesList.toTermArray()) {
                    Term[] details = sale.toTermArray();
                    Purchase purchase = new Purchase(
                        details[1].name(),      // Date
                        details[2].doubleValue(), // PreDiscountAmount
                        details[3].doubleValue(), // CategoryDiscount
                        details[4].doubleValue(), // LoyaltyDiscount
                        details[5].doubleValue(), // ShippingCost
                        details[6].doubleValue()  // Total
                    );
                    purchases.add(purchase);
                }
            }
        }
        return purchases;
    }

    /**
     * Processes a query to extract total sales data and displays them.
     * The Prolog query object configured to fetch total sales data.
     */
    private static void processTotalsQuery(Query query) {
        if (query.hasSolution()) {
            Map<String, Term> solution = query.oneSolution();
            Term totals = solution.get("Totals");
            Term[] details = totals.toTermArray();
            System.out.printf("Total PreDiscount: %.2f, Total CategoryDiscount: %.2f, Total LoyaltyDiscount: %.2f, Total ShippingCost: %.2f, Total: %.2f\n",
                    details[0].doubleValue(), details[1].doubleValue(), details[2].doubleValue(), details[3].doubleValue(), details[4].doubleValue());
        } else {
            System.out.println("No totals found.");
        }
    }

    //Displays a list of purchases in a readable format.
    private static void displayPurchases(List<Purchase> purchases) {
        if (purchases.isEmpty()) {
            System.out.println("No sales found.");
        } else {
            // purchases List of purchases to be displayed.
            for (Purchase purchase : purchases) {
                System.out.println(purchase);
            }
        }
    }
}
