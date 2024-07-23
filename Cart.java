import java.util.ArrayList;
import java.util.List;

// Public class that can be accessed by other classes.
public class Cart {
    // Private List of Item objects, accessible only within this class.
    private List<Item> items;
    
    // Getter method for items. Returns the list of items in the cart.
    public List<Item> getItems() {
        return this.items;
    }

    // Constructor for the Cart class. Initializes the items list as an empty ArrayList.
    public Cart() { 
        this.items = new ArrayList<>();
    }

    // Method to add an item to the cart. The item is added to the items list.
    public void addItem(Item item) {
        items.add(item);
    }

    // Generates a formatted string representation of all items in the cart for use in CartOperations.
    // Useful for converting the cart's contents into a string format that can be easily read or processed.
    public String generateCartString() {
        StringBuilder sb = new StringBuilder(); // StringBuilder to construct the string efficiently.
        for (Item item : items) { // Iterate over each item in the list.
            if (sb.length() > 0) sb.append(","); // Append a comma before every item except the first for proper formatting.
            sb.append(item.formatForCart()); // Append the formatted string of the item.
        }
        return sb.toString(); // Return the complete formatted string of the cart's contents.
    }
}
