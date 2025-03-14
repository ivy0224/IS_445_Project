# I S 445 - Database Management Project
# Hotel Cancellation Analysis
## Exploratory Questions(SQL and Tableau)
  - **Question 1**: How do special requests affect hotel cancellation rates? 
  - **Question 2**: How does the cancellation rate vary across different seasons?
  - **Question 3**: What effect do hotel market segment types have on booking cancellations? 
  - **Question 4**: How far ahead should customers book their reservation?
## Objective of Modeling (RStudio)
- **Goal**: Predict whether a future booking will be canceled or not
- **Target(Y)**: "booking status"
- **Predictors(X)**: 16 potential variables such as "lead time", "special requests", etc.
- **Models**
  -  ***Logistic Regression***
  -  ***Default Decision Tree***
  -  ***Decision Tree with Control***
  -  ***Random Forest Tree Ensemble***
## Key Takeaways and Insights
- **Business Implication**
  - **Special Requests**
    - The higher the special requests, the less cancellation possibility
    - Invest in customer relationship management (CRM) and offer loyalty programs
      - Retention rates may increase
      - Deciding factor over competitors
  - **Months/Seasons**
    - Summer seasons are highest in cancellation
    - Based on seasons, hotels can reduce revenue loss and optimize occupancy by overbooking strategically 
      - Minimizes empty rooms at a certain time
  - **Market Segment Type**
    - Pricing Model Adjustment
      - Charge corporate segments higher as they are more inelastic
    - Marketing Strategies
      - Focus on target audience: low cancellation segments such as "Corporate" and "Aviation"
- **Customer Implication**
  - Avoid uncertainty in trip planning
  - If booking 1+ month in advance, look for flexible cancellation policies
## Data
- **Source**: https://www.kaggle.com/datasets/youssefaboelwafa/hotel-booking-cancellation-prediction/data
- **Description**: The dataset contains booking data for hotels, including information on customer demographics, booking dates, and whether the booking was canceled.
  - **Number of Columns**: 16
  - **Number of Records**: 36,285
  - **Collected Time**: 7/1/2015 to 8/31/2017
- **Columns**: 
  - `Booking_ID`: Unique identifier for each booking
  - `number of adults`: Number of adults included in the booking
  - `number of children`: Number of children included in the booking
  - `number of weekend nights`: Number of weekend nights included in the booking
  - `number of week nights`: Number of week nights included in the booking
  - `type of meal`: Type of meal included in the booking
  - `car parking space`: Indicates whether a car parking space was requested or included in the booking
  - `room type`: Type of room booked
  - `lead time`: Number of days between the booking date and the arrival date
  - `market segment type`: Type of market segment associated with the booking
  - `repeated`: Indicates whether the booking is a repeat booking
  - `P-C`: Number of previous bookings that were canceled by the customer prior to the current booking
  - `P-not-C`: Number of previous bookings not canceled by the customer prior to the current booking
  - `average price`: Average price associated with the booking
  - `special requests`: Number of special requests made by the guest
  - `date of reservation`: Date of the reservation
  - `booking status`: Status of the booking ( canceled or not canceled)
