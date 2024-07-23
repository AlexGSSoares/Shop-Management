/**********************************************************
 ***************          EFOLIO B         ****************
 **********      Linguagens de Programação      ***********
 **********************************************************
 ***********    Professor: RICARDO BAPTISTA     ***********
 ***********   Monitor: RÚDI GUALTER OLIVEIRA   ***********
 ************     Aluno: Alexandre Soares      ************
 *************      Nr de Aluno: 2101521      *************
 *********************************************************/

import org.jpl7.*;
import java.util.Scanner;

public class Main {
    // Reade user Entrance
    private static Scanner scanner = new Scanner(System.in);
    // Path to Prolog File
    private static final String prologFile = "store.pl";

    
    public static void main(String[] args) {
        //Instance of the Store class, with scanner as argument
        Store store = new Store(scanner);
        //Creates the Prolog query and checks if the query was successful
        Query q1 = new Query("consult", new Term[]{new Atom(prologFile)});
        System.out.println("Consult " + (q1.hasSolution() ? "succeeded" : "FAILED"));

        //Main Menu. 'do-while' executes until receive 0 to exit
        int mainChoice;
        do {
            System.out.println("\nMain Menu:");
            System.out.println("1 - Store");
            System.out.println("2 - Administration");
            System.out.println("0 - Exit");
            System.out.print("Enter your choice: ");
            mainChoice = scanner.nextInt();
            scanner.nextLine();  // Consume newline

            switch (mainChoice) {
                case 1:
                    store.runStore();
                    break;
                case 2:
                    showAdminMenu();
                    break;
                case 0:
                    System.out.println("Exiting...");
                    break;
                default:
                    System.out.println("Invalid choice, please choose again.");
            }
        } while (mainChoice != 0);
        scanner.close();
    }

    private static void showAdminMenu() {
        //Instance of the classes class, with scanner as argument
        ManageSalesHistory salesHistory = new ManageSalesHistory(scanner);
        ManageInventory inventoryManager = new ManageInventory(scanner);
        ManageCostsAndDiscounts costsAndDiscounts = new ManageCostsAndDiscounts(scanner);
        ManageClient client = new ManageClient(scanner);

        //Sub-Menu
        int choice;
        do {
            System.out.println("\nAdministration:");
            System.out.println("1 - Manage Clients");
            System.out.println("2 - Manage Costs and Discounts");
            System.out.println("3 - Manage Inventory");
            System.out.println("4 - Manage Sales History");
            System.out.println("0 - Return to Main Menu");
            System.out.print("Enter your choice: ");
            choice = scanner.nextInt();
            scanner.nextLine();  // Consume newline

            //Calls the java classes
            switch (choice) {
                case 1:
                    client.runManageclient ();
                    break;
                case 2:
                    costsAndDiscounts.runCostsAndDiscountsManager();
                    break;
                case 3:
                    inventoryManager.runInventoryManager();
                    break;
                case 4:
                    salesHistory.displayMenu();
                    break;
                case 0:
                    System.out.println("Returning to Main Menu...");
                    break;
                default:
                    System.out.println("Invalid choice, please choose again.");
            }
        } while (choice != 0);
    }
}