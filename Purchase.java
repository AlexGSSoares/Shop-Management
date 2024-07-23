/**
 * Represents a sales purchase record.
 * It includes details such as the date of the purchase, amounts before and after discounts,
 * specific discounts applied, and the shipping cost involved.
 */

public class Purchase {
    private String date;
    private double preDiscountAmount;
    private double categoryDiscount;
    private double loyaltyDiscount;
    private double shippingCost;
    private double total;

    // Constructs a Purchase object with detailed information about a transaction.
     
    public Purchase(String date, double preDiscountAmount, double categoryDiscount, double loyaltyDiscount, double shippingCost, double total) {
        this.date = date;
        this.preDiscountAmount = preDiscountAmount;
        this.categoryDiscount = categoryDiscount;
        this.loyaltyDiscount = loyaltyDiscount;
        this.shippingCost = shippingCost;
        this.total = total;
    }

    // Getters
    public String getDate() {
        return date;
    }

    public double getPreDiscountAmount() {
        return preDiscountAmount;
    }

    public double getCategoryDiscount() {
        return categoryDiscount;
    }

    public double getLoyaltyDiscount() {
        return loyaltyDiscount;
    }

    public double getShippingCost() {
        return shippingCost;
    }

    public double getTotal() {
        return total;
    }

    // Setters
    public void setDate(String date) {
        this.date = date;
    }

    public void setPreDiscountAmount(double preDiscountAmount) {
        this.preDiscountAmount = preDiscountAmount;
    }

    public void setCategoryDiscount(double categoryDiscount) {
        this.categoryDiscount = categoryDiscount;
    }

    public void setLoyaltyDiscount(double loyaltyDiscount) {
        this.loyaltyDiscount = loyaltyDiscount;
    }

    public void setShippingCost(double shippingCost) {
        this.shippingCost = shippingCost;
    }

    public void setTotal(double total) {
        this.total = total;
    }

    //Provides a string representation of the purchase details, formatted for readability.
    // A formatted string describing the purchase.
    @Override
    public String toString() {
        return String.format("Date: %s, Amount Before Discount: %.2f, Category Discount: %.2f, Loyalty Discount: %.2f, Shipping Cost: %.2f, Total: %.2f",
                             date, preDiscountAmount, categoryDiscount, loyaltyDiscount, shippingCost, total);
    }
}
