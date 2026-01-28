-- 创建示例数据表
CREATE TABLE IF NOT EXISTS sample_data (
    id INT AUTO_INCREMENT PRIMARY KEY,
    date DATE NOT NULL,
    category VARCHAR(50),
    value1 DECIMAL(10, 2),
    value2 DECIMAL(10, 2),
    value3 DECIMAL(10, 2),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- 插入示例数据
INSERT INTO sample_data (date, category, value1, value2, value3) VALUES
('2023-01-01', 'A', 100.50, 200.75, 300.25),
('2023-01-02', 'B', 150.25, 250.50, 350.75),
('2023-01-03', 'A', 120.75, 220.25, 320.50),
('2023-01-04', 'C', 180.50, 280.75, 380.25),
('2023-01-05', 'B', 130.25, 230.50, 330.75),
('2023-01-06', 'A', 140.75, 240.25, 340.50),
('2023-01-07', 'C', 160.50, 260.75, 360.25),
('2023-01-08', 'B', 170.25, 270.50, 370.75),
('2023-01-09', 'A', 190.75, 290.25, 390.50),
('2023-01-10', 'C', 110.50, 210.75, 310.25);

-- 创建用户表（如果不存在）
CREATE TABLE IF NOT EXISTS app_users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    username VARCHAR(50) UNIQUE NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    email VARCHAR(100),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_login TIMESTAMP NULL,
    is_active BOOLEAN DEFAULT TRUE
);

-- 创建数据分析结果表
CREATE TABLE IF NOT EXISTS analysis_results (
    id INT AUTO_INCREMENT PRIMARY KEY,
    user_id INT,
    analysis_type VARCHAR(50),
    input_data JSON,
    result_data JSON,
    compute_engine VARCHAR(20),
    execution_time DECIMAL(10, 4),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES app_users(id)
);

-- 创建索引
CREATE INDEX idx_sample_data_date ON sample_data(date);
CREATE INDEX idx_sample_data_category ON sample_data(category);
CREATE INDEX idx_analysis_results_user ON analysis_results(user_id);
CREATE INDEX idx_analysis_results_created ON analysis_results(created_at);