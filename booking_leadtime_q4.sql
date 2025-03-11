#Question 4- Lead Time Code

#make lead time category to seperate into weeks and months 
WITH LeadTimeBuckets AS (
    SELECT 
        CASE 
            WHEN lead_time <= 7 THEN '0-7 Days'
            WHEN lead_time BETWEEN 8 AND 30 THEN '8-30 Days'
            WHEN lead_time BETWEEN 31 AND 90 THEN '31-90 Days'
            WHEN lead_time BETWEEN 91 AND 180 THEN '91-180 Days'
            ELSE '181+ Days'
        END AS lead_time_category,
        booking_status
    FROM bookingnew
)

#creating cancellation rate in comparisoin to lead time 
SELECT 
    lead_time_category,
    COUNT(*) AS total_bookings,
    SUM(CASE WHEN booking_status = 'Canceled' THEN 1 ELSE 0 END) AS canceled_bookings,
    ROUND(
        (SUM(CASE WHEN booking_status = 'Canceled' THEN 1 ELSE 0 END) * 100.0) / COUNT(*), 2
    ) AS cancellation_rate
FROM LeadTimeBuckets
GROUP BY lead_time_category
ORDER BY cancellation_rate asc;





