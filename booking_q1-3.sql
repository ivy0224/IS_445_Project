Create SCHEMA hotel_booking;

SELECT `date of reservation` FROM booking;

UPDATE booking
SET `date of reservation` = STR_TO_DATE(`date of reservation`, '%m/%d/%Y')
WHERE STR_TO_DATE(`date of reservation`, '%m/%d/%Y') IS NOT NULL;

SELECT `date of reservation`
FROM booking
WHERE STR_TO_DATE(`date of reservation`, '%m/%d/%Y') IS NULL;

DELETE FROM booking
WHERE STR_TO_DATE(`date of reservation`, '%m/%d/%Y') IS NULL;

DELETE FROM booking
WHERE `date of reservation` NOT REGEXP '^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$';

UPDATE booking
SET reservation_month = MONTH(STR_TO_DATE(`date of reservation`, '%Y-%m-%d'));

SELECT 
    reservation_month,
    COUNT(*) AS total_bookings,
    SUM(CASE WHEN `booking status` = 'canceled' THEN 1 ELSE 0 END) AS total_cancellations
FROM 
    booking
GROUP BY 
    reservation_month
ORDER BY 
    total_bookings DESC;
    
SELECT 
    reservation_month,
    COUNT(*) AS total_bookings,
    SUM(CASE WHEN `booking status` = 'canceled' THEN 1 ELSE 0 END) AS total_cancellations,
    ROUND(SUM(CASE WHEN `booking status` = 'canceled' THEN 1 ELSE 0 END) / COUNT(*) * 100, 2) AS cancellation_rate
FROM 
    booking
GROUP BY 
    reservation_month
ORDER BY 
    cancellation_rate DESC;
    
SELECT 
    `special requests`,
    COUNT(*) AS total_bookings,
    SUM(CASE WHEN `booking status` = 'canceled' THEN 1 ELSE 0 END) AS total_cancellations,
    ROUND(SUM(CASE WHEN `booking status` = 'canceled' THEN 1 ELSE 0 END) / COUNT(*) * 100, 2) AS cancellation_rate
FROM booking GROUP BY `special requests` ORDER BY cancellation_rate DESC;

SELECT 
    `market segment type`,
    COUNT(*) AS total_bookings,
    SUM(CASE WHEN `booking status` = 'canceled' THEN 1 ELSE 0 END) AS total_cancellations,
    ROUND(SUM(CASE WHEN `booking status` = 'canceled' THEN 1 ELSE 0 END) / COUNT(*) * 100, 2) AS cancellation_rate
FROM booking GROUP BY `market segment type` ORDER BY cancellation_rate DESC;

SELECT 
    `average price`,
    COUNT(*) AS total_bookings,
    SUM(CASE WHEN `booking status` = 'canceled' THEN 1 ELSE 0 END) AS total_cancellations,
    ROUND(SUM(CASE WHEN `booking status` = 'canceled' THEN 1 ELSE 0 END) / COUNT(*) * 100, 2) AS cancellation_rate
FROM booking GROUP BY `average price` ORDER BY cancellation_rate DESC;
    
SELECT 
    CASE 
        WHEN `average price` <= 50 THEN '0-50'
        WHEN `average price` <= 100 THEN '51-100'
        WHEN `average price` <= 150 THEN '101-150'
        WHEN `average price` <= 200 THEN '151-200'
        ELSE '200+'
    END AS price_range,
    COUNT(*) AS total_bookings,
    SUM(CASE WHEN LOWER(`booking status`) = 'canceled' THEN 1 ELSE 0 END) AS canceled_bookings,
    ROUND(SUM(CASE WHEN LOWER(`booking status`) = 'canceled' THEN 1 ELSE 0 END) / COUNT(*) * 100, 2) AS cancellation_rate
FROM booking
GROUP BY price_range
ORDER BY cancellation_rate;

SELECT 
    `type of meal`,
    COUNT(*) AS total_bookings,
    SUM(CASE WHEN `booking status` = 'canceled' THEN 1 ELSE 0 END) AS total_cancellations,
    ROUND(SUM(CASE WHEN `booking status` = 'canceled' THEN 1 ELSE 0 END) / COUNT(*) * 100, 2) AS cancellation_rate
FROM booking GROUP BY `type of meal` ORDER BY cancellation_rate DESC;

SELECT AVG(`lead time`) FROM booking;

SELECT 
    CASE 
        WHEN `lead time` <= 15 THEN '0-15'
        WHEN `lead time` <= 50 THEN '15-50'
        WHEN `lead time` <= 100 THEN '50-100'
        WHEN `lead time` <= 150 THEN '100-150'
        ELSE '150+'
    END AS lead_time_range,
    COUNT(*) AS total_bookings,
    SUM(CASE WHEN LOWER(`booking status`) = 'canceled' THEN 1 ELSE 0 END) AS canceled_bookings,
    ROUND(SUM(CASE WHEN LOWER(`booking status`) = 'canceled' THEN 1 ELSE 0 END) / COUNT(*) * 100, 2) AS cancellation_rate
FROM booking
GROUP BY lead_time_range
ORDER BY cancellation_rate;
