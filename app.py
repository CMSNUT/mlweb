#!/usr/bin/env python3
"""
mlweb - Streamlit机器学习平台主应用
集成R Plumber API，提供完整的机器学习工作流
"""

import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objs as go
from plotly.subplots import make_subplots
import plotly.express as px
import requests
import json
import time
import os
import sys
import subprocess
import tempfile
import base64
import io
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple
import asyncio
import aiohttp
from pathlib import Path
import logging
from logging.handlers import RotatingFileHandler
from dataclasses import dataclass
from enum import Enum
import warnings
warnings.filterwarnings('ignore')

# 配置日志
def setup_logging():
    """配置应用日志"""
    log_dir = Path("logs")
    log_dir.mkdir(exist_ok=True)
    
    log_file = log_dir / "app.log"
    
    # 配置根日志
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            RotatingFileHandler(log_file, maxBytes=10*1024*1024, backupCount=5),
            logging.StreamHandler()
        ]
    )
    
    return logging.getLogger(__name__)

logger = setup_logging()

# 页面配置
st.set_page_config(
    page_title="mlweb - 机器学习平台",
    page_icon="🤖",
    layout="wide",
    initial_sidebar_state="expanded",
    menu_items={
        'Get Help': 'https://github.com/yourusername/mlweb',
        'Report a bug': 'https://github.com/yourusername/mlweb/issues',
        'About': """
        # mlweb机器学习平台
        
        一个集成Streamlit和R Plumber API的端到端机器学习平台。
        
        **版本**: 1.0.0
        **许可证**: MIT
        """
    }
)

# 自定义CSS
def load_css():
    """加载自定义CSS样式"""
    css_file = Path("static/css/style.css")
    
    if css_file.exists():
        try:
            with open(css_file, 'r', encoding='utf-8') as f:
                css_content = f.read()
            st.markdown(f'<style>{css_content}</style>', unsafe_allow_html=True)
            logger.info("CSS文件加载成功")
        except Exception as e:
            logger.error(f"加载CSS文件失败: {e}")
            # 如果文件加载失败，使用默认样式
            load_default_css()
    else:
        logger.warning(f"CSS文件不存在: {css_file}")
        load_default_css()

def load_default_css():
    """加载默认CSS样式（备用）"""
    default_css = """
    /* 基础样式确保应用可运行 */
    .main-title {
        font-size: 2.5rem;
        font-weight: bold;
        color: #1E88E5;
        margin-bottom: 1rem;
    }
    .section-title {
        font-size: 1.8rem;
        font-weight: bold;
        color: #3949AB;
        margin-top: 2rem;
        margin-bottom: 1rem;
        padding-bottom: 0.5rem;
        border-bottom: 2px solid #E3F2FD;
    }
    """
    st.markdown(f'<style>{default_css}</style>', unsafe_allow_html=True)

# 应用配置类
@dataclass
class AppConfig:
    """应用配置"""
    API_BASE_URL: str = "http://localhost:8000"
    API_TIMEOUT: int = 60
    MAX_FILE_SIZE: int = 100 * 1024 * 1024  # 100MB
    SUPPORTED_FILE_TYPES: List[str] = None
    CACHE_DIR: str = ".cache"
    
    def __post_init__(self):
        if self.SUPPORTED_FILE_TYPES is None:
            self.SUPPORTED_FILE_TYPES = [
                "csv", "xlsx", "xls", "json", "parquet", 
                "feather", "pickle", "pkl", "txt"
            ]

# 会话状态管理
class SessionState:
    """管理Streamlit会话状态"""
    def __init__(self):
        # 应用状态
        self.current_step = 1
        self.steps = {
            1: "📊 数据加载",
            2: "🔍 数据探索",
            3: "🧹 数据预处理",
            4: "🤖 模型训练",
            5: "📈 模型评估",
            6: "🚀 模型部署",
            7: "⚡ 实时预测"
        }
        
        # 数据状态
        self.data_loaded = False
        self.raw_data = None
        self.processed_data = None
        self.train_data = None
        self.test_data = None
        self.target_column = None
        self.problem_type = "classification"  # "classification" 或 "regression"
        
        # 模型状态
        self.models_trained = False
        self.training_result = None
        self.selected_model = None
        self.evaluation_result = None
        
        # API状态
        self.api_available = False
        self.api_status = {}
        self.last_api_check = None
        
        # 缓存
        self.cache = {}
        
        # 日志
        self.logs = []
        
    def reset(self):
        """重置会话状态"""
        self.__init__()
        
    def log(self, message: str, level: str = "INFO"):
        """记录日志"""
        timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        self.logs.append({
            "timestamp": timestamp,
            "level": level,
            "message": message
        })
        logger.log(getattr(logging, level), message)
        
    def get_logs(self, level: str = None):
        """获取日志"""
        if level:
            return [log for log in self.logs if log["level"] == level]
        return self.logs

# 初始化
load_css()
config = AppConfig()
state = SessionState()

# API客户端
class APIClient:
    """R Plumber API客户端"""
    
    def __init__(self, base_url: str = None, timeout: int = 60):
        self.base_url = base_url or config.API_BASE_URL
        self.timeout = timeout
        self.session = None
        self.stats = {
            "total_requests": 0,
            "successful_requests": 0,
            "failed_requests": 0,
            "total_time": 0
        }
        
    async def __aenter__(self):
        """异步上下文管理器入口"""
        self.session = aiohttp.ClientSession()
        return self
        
    async def __aexit__(self, exc_type, exc_val, exc_tb):
        """异步上下文管理器出口"""
        if self.session:
            await self.session.close()
            
    def _make_url(self, endpoint: str) -> str:
        """构建完整URL"""
        return f"{self.base_url}/{endpoint.lstrip('/')}"
    
    def check_health(self) -> Dict:
        """检查API健康状态"""
        try:
            start_time = time.time()
            response = requests.get(
                self._make_url("health"),
                timeout=5
            )
            elapsed = time.time() - start_time
            
            self.stats["total_requests"] += 1
            if response.status_code == 200:
                self.stats["successful_requests"] += 1
                status = response.json()
                status["response_time"] = elapsed
                return status
            else:
                self.stats["failed_requests"] += 1
                return {
                    "status": "unhealthy",
                    "error": f"HTTP {response.status_code}",
                    "response_time": elapsed
                }
        except Exception as e:
            self.stats["failed_requests"] += 1
            return {
                "status": "unreachable",
                "error": str(e),
                "response_time": 0
            }
    
    async def async_request(self, method: str, endpoint: str, **kwargs) -> Dict:
        """异步HTTP请求"""
        if not self.session:
            self.session = aiohttp.ClientSession()
            
        url = self._make_url(endpoint)
        start_time = time.time()
        
        try:
            async with self.session.request(method, url, **kwargs) as response:
                elapsed = time.time() - start_time
                self.stats["total_requests"] += 1
                
                if response.status == 200:
                    self.stats["successful_requests"] += 1
                    data = await response.json()
                    data["response_time"] = elapsed
                    return data
                else:
                    self.stats["failed_requests"] += 1
                    error_text = await response.text()
                    return {
                        "success": False,
                        "error": f"HTTP {response.status}: {error_text}",
                        "response_time": elapsed
                    }
        except Exception as e:
            self.stats["failed_requests"] += 1
            return {
                "success": False,
                "error": str(e),
                "response_time": time.time() - start_time
            }
    
    def sync_request(self, method: str, endpoint: str, **kwargs) -> Dict:
        """同步HTTP请求"""
        url = self._make_url(endpoint)
        start_time = time.time()
        
        try:
            response = requests.request(
                method, url,
                timeout=self.timeout,
                **kwargs
            )
            elapsed = time.time() - start_time
            self.stats["total_requests"] += 1
            
            if response.status_code == 200:
                self.stats["successful_requests"] += 1
                data = response.json()
                data["response_time"] = elapsed
                return data
            else:
                self.stats["failed_requests"] += 1
                return {
                    "success": False,
                    "error": f"HTTP {response.status_code}: {response.text}",
                    "response_time": elapsed
                }
        except Exception as e:
            self.stats["failed_requests"] += 1
            return {
                "success": False,
                "error": str(e),
                "response_time": time.time() - start_time
            }
    
    def get_stats(self) -> Dict:
        """获取API统计信息"""
        return {
            **self.stats,
            "success_rate": (
                self.stats["successful_requests"] / self.stats["total_requests"] 
                if self.stats["total_requests"] > 0 else 0
            )
        }

# 数据处理器
class DataProcessor:
    """数据处理工具类"""
    
    @staticmethod
    def detect_file_type(file_path: str) -> str:
        """检测文件类型"""
        ext = Path(file_path).suffix.lower().lstrip('.')
        return ext
    
    @staticmethod
    def load_file(file_path: str, file_type: str = None) -> pd.DataFrame:
        """加载文件为DataFrame"""
        if file_type is None:
            file_type = DataProcessor.detect_file_type(file_path)
            
        try:
            if file_type == 'csv':
                return pd.read_csv(file_path)
            elif file_type in ['xlsx', 'xls']:
                return pd.read_excel(file_path)
            elif file_type == 'json':
                return pd.read_json(file_path)
            elif file_type == 'parquet':
                return pd.read_parquet(file_path)
            elif file_type == 'feather':
                return pd.read_feather(file_path)
            elif file_type in ['pickle', 'pkl']:
                return pd.read_pickle(file_path)
            elif file_type == 'txt':
                return pd.read_csv(file_path, sep='\t')
            else:
                raise ValueError(f"不支持的文件类型: {file_type}")
        except Exception as e:
            logger.error(f"加载文件失败: {e}")
            raise
    
    @staticmethod
    def analyze_data(df: pd.DataFrame) -> Dict:
        """分析数据特征"""
        analysis = {
            "shape": df.shape,
            "dtypes": df.dtypes.astype(str).to_dict(),
            "missing_values": df.isnull().sum().to_dict(),
            "missing_percentage": (df.isnull().sum() / len(df) * 100).to_dict(),
            "numeric_stats": {},
            "categorical_stats": {},
            "correlations": {}
        }
        
        # 数值列统计
        numeric_cols = df.select_dtypes(include=[np.number]).columns
        if len(numeric_cols) > 0:
            analysis["numeric_stats"] = df[numeric_cols].describe().to_dict()
            
            # 相关性矩阵
            if len(numeric_cols) > 1:
                analysis["correlations"] = df[numeric_cols].corr().to_dict()
        
        # 分类列统计
        categorical_cols = df.select_dtypes(include=['object', 'category']).columns
        for col in categorical_cols:
            analysis["categorical_stats"][col] = {
                "unique_values": df[col].nunique(),
                "value_counts": df[col].value_counts().head(10).to_dict()
            }
        
        return analysis
    
    @staticmethod
    def create_data_visualizations(df: pd.DataFrame) -> Dict:
        """创建数据可视化"""
        viz = {}
        
        # 数值列分布
        numeric_cols = df.select_dtypes(include=[np.number]).columns
        if len(numeric_cols) > 0:
            for col in numeric_cols[:5]:  # 限制前5个数值列
                fig = px.histogram(df, x=col, nbins=50, title=f"{col}分布")
                viz[f"hist_{col}"] = fig
        
        # 相关性热图
        if len(numeric_cols) > 1:
            corr_matrix = df[numeric_cols].corr()
            fig = px.imshow(
                corr_matrix,
                title="特征相关性热图",
                color_continuous_scale="RdBu"
            )
            viz["correlation_heatmap"] = fig
        
        # 散点图矩阵
        if len(numeric_cols) >= 2:
            fig = px.scatter_matrix(
                df[numeric_cols[:4]],  # 限制前4个特征
                title="散点图矩阵"
            )
            viz["scatter_matrix"] = fig
        
        return viz

# 模型管理器
class ModelManager:
    """模型管理工具类"""
    
    @staticmethod
    def get_model_info(model_result: Dict) -> Dict:
        """提取模型信息"""
        info = {
            "name": model_result.get("name", "未知模型"),
            "type": model_result.get("type", "未知类型"),
            "performance": {},
            "parameters": {},
            "training_info": {}
        }
        
        # 提取性能指标
        if "performance" in model_result:
            perf = model_result["performance"]
            info["performance"] = {
                "accuracy": perf.get("accuracy"),
                "precision": perf.get("precision"),
                "recall": perf.get("recall"),
                "f1": perf.get("f1"),
                "auc": perf.get("auc"),
                "rmse": perf.get("rmse"),
                "mae": perf.get("mae"),
                "r2": perf.get("r2")
            }
        
        # 提取参数
        if "parameters" in model_result:
            info["parameters"] = model_result["parameters"]
            
        # 提取训练信息
        if "training_info" in model_result:
            info["training_info"] = model_result["training_info"]
            
        return info

# 页面组件
class PageComponents:
    """页面组件工厂"""
    
    @staticmethod
    def create_header():
        """创建页面头部"""
        st.markdown('<h1 class="main-title fade-in">🤖 mlweb机器学习平台</h1>', 
                   unsafe_allow_html=True)
        st.markdown("""
        <div class="alert alert-info">
        <strong>🚀 端到端机器学习平台</strong> - 集成数据探索、模型训练、评估和部署的全流程工具
        </div>
        """, unsafe_allow_html=True)
    
    @staticmethod
    def create_sidebar():
        """创建侧边栏"""
        with st.sidebar:
            st.image("static/images/logo.png", width=200)
            
            st.markdown("## 🧭 导航")
            for step_num, step_name in state.steps.items():
                col1, col2 = st.columns([1, 4])
                with col1:
                    st.markdown(f"**{step_num}**")
                with col2:
                    if st.button(
                        step_name,
                        key=f"nav_{step_num}",
                        use_container_width=True,
                        disabled=(step_num > state.current_step)
                    ):
                        state.current_step = step_num
                        st.rerun()
            
            st.divider()
            
            # 系统状态
            st.markdown("## 📊 系统状态")
            
            # API状态
            api_status_color = {
                "healthy": "🟢",
                "unhealthy": "🟡",
                "unreachable": "🔴"
            }
            api_status = state.api_status.get("status", "unknown")
            
            col1, col2 = st.columns(2)
            with col1:
                st.metric("API状态", api_status_color.get(api_status, "⚪"))
            with col2:
                st.metric("当前步骤", f"{state.current_step}/7")
            
            if state.api_available:
                response_time = state.api_status.get("response_time", 0)
                st.caption(f"API响应时间: {response_time:.3f}秒")
            
            st.divider()
            
            # 工具
            st.markdown("## 🛠️ 工具")
            
            if st.button("🔄 重置会话", use_container_width=True):
                state.reset()
                st.rerun()
                
            if st.button("📊 查看日志", use_container_width=True):
                PageComponents.show_logs()
                
            if st.button("⚙️ 系统设置", use_container_width=True):
                PageComponents.show_settings()
    
    @staticmethod
    def show_logs():
        """显示日志窗口"""
        with st.expander("📋 系统日志", expanded=True):
            logs = state.get_logs()
            if logs:
                for log in reversed(logs[-10:]):  # 显示最近10条日志
                    level_color = {
                        "INFO": "blue",
                        "WARNING": "orange",
                        "ERROR": "red",
                        "DEBUG": "gray"
                    }
                    st.markdown(
                        f'<span style="color: {level_color.get(log["level"], "black")}">'
                        f'[{log["timestamp"]}] {log["level"]}: {log["message"]}'
                        f'</span>',
                        unsafe_allow_html=True
                    )
            else:
                st.info("暂无日志")
    
    @staticmethod
    def show_settings():
        """显示系统设置"""
        with st.expander("⚙️ 系统设置", expanded=True):
            st.subheader("API设置")
            
            api_url = st.text_input("API地址", value=config.API_BASE_URL)
            timeout = st.number_input("超时时间(秒)", min_value=1, max_value=300, value=config.API_TIMEOUT)
            
            if st.button("保存设置"):
                config.API_BASE_URL = api_url
                config.API_TIMEOUT = timeout
                st.success("设置已保存")
            
            st.divider()
            
            st.subheader("数据设置")
            max_file_size = st.number_input("最大文件大小(MB)", min_value=1, max_value=1000, 
                                           value=config.MAX_FILE_SIZE // (1024*1024))
            
            if st.button("测试API连接"):
                client = APIClient()
                status = client.check_health()
                state.api_status = status
                state.api_available = (status.get("status") == "healthy")
                
                if state.api_available:
                    st.success("✅ API连接正常")
                else:
                    st.error(f"❌ API连接失败: {status.get('error', '未知错误')}")
    
    @staticmethod
    def create_progress_bar():
        """创建进度条"""
        progress = state.current_step / len(state.steps)
        
        st.markdown("""
        <div class="progress-container">
            <div class="progress-bar" style="width: {:.1%}"></div>
        </div>
        """.format(progress), unsafe_allow_html=True)
        
        col1, col2, col3, col4, col5, col6, col7 = st.columns(7)
        steps_list = list(state.steps.values())
        for i, (col, step_name) in enumerate(zip([col1, col2, col3, col4, col5, col6, col7], steps_list), 1):
            with col:
                if i < state.current_step:
                    st.markdown(f"✅ {step_name.split(' ')[1]}")
                elif i == state.current_step:
                    st.markdown(f"📍 **{step_name.split(' ')[1]}**")
                else:
                    st.markdown(f"⚪ {step_name.split(' ')[1]}")
    
    @staticmethod
    def create_footer():
        """创建页脚"""
        st.divider()
        col1, col2, col3 = st.columns(3)
        
        with col1:
            st.markdown("**版本**: 1.0.0")
        with col2:
            st.markdown("**最后更新**: 2024-01-01")
        with col3:
            st.markdown("**状态**: 🟢 在线")

# 页面控制器
class PageController:
    """页面控制器"""
    
    def __init__(self):
        self.pages = {
            1: self.page_data_loading,
            2: self.page_data_exploration,
            3: self.page_data_preprocessing,
            4: self.page_model_training,
            5: self.page_model_evaluation,
            6: self.page_model_deployment,
            7: self.page_realtime_prediction
        }
    
    def render(self):
        """渲染当前页面"""
        PageComponents.create_header()
        PageComponents.create_sidebar()
        PageComponents.create_progress_bar()
        
        # 显示当前页面
        page_func = self.pages.get(state.current_step)
        if page_func:
            page_func()
        else:
            st.error("页面不存在")
            
        PageComponents.create_footer()
    
    def page_data_loading(self):
        """数据加载页面"""
        st.markdown('<h2 class="section-title">📊 数据加载</h2>', 
                   unsafe_allow_html=True)
        
        # 数据源选择
        data_source = st.radio(
            "选择数据源",
            ["上传文件", "示例数据", "数据库连接", "API接口"],
            horizontal=True
        )
        
        if data_source == "上传文件":
            self._handle_file_upload()
        elif data_source == "示例数据":
            self._handle_example_data()
        elif data_source == "数据库连接":
            self._handle_database_connection()
        else:
            self._handle_api_source()
    
    def _handle_file_upload(self):
        """处理文件上传"""
        uploaded_file = st.file_uploader(
            "选择数据文件",
            type=config.SUPPORTED_FILE_TYPES,
            help=f"支持的文件类型: {', '.join(config.SUPPORTED_FILE_TYPES)}"
        )
        
        if uploaded_file is not None:
            # 检查文件大小
            file_size = len(uploaded_file.getvalue())
            if file_size > config.MAX_FILE_SIZE:
                st.error(f"文件大小超过限制 ({file_size/(1024*1024):.1f}MB > "
                        f"{config.MAX_FILE_SIZE/(1024*1024):.1f}MB)")
                return
            
            # 保存临时文件
            with tempfile.NamedTemporaryFile(delete=False, suffix=f".{uploaded_file.name.split('.')[-1]}") as tmp_file:
                tmp_file.write(uploaded_file.getvalue())
                tmp_path = tmp_file.name
            
            try:
                # 加载数据
                with st.spinner("正在加载数据..."):
                    df = DataProcessor.load_file(tmp_path)
                    state.raw_data = df
                    state.data_loaded = True
                    
                    st.success(f"✅ 数据加载成功: {df.shape[0]} 行 × {df.shape[1]} 列")
                    
                    # 显示数据预览
                    with st.expander("📋 数据预览", expanded=True):
                        tab1, tab2, tab3 = st.tabs(["数据头", "数据尾", "随机样本"])
                        with tab1:
                            st.dataframe(df.head(10), use_container_width=True)
                        with tab2:
                            st.dataframe(df.tail(10), use_container_width=True)
                        with tab3:
                            st.dataframe(df.sample(10), use_container_width=True)
                    
                    # 数据信息
                    with st.expander("📊 数据信息", expanded=False):
                        st.write(f"**数据类型**:")
                        for col, dtype in df.dtypes.items():
                            st.write(f"- {col}: {dtype}")
                    
                    # 下一步按钮
                    if st.button("下一步: 数据探索", type="primary", use_container_width=True):
                        state.current_step = 2
                        st.rerun()
                        
            except Exception as e:
                st.error(f"加载数据失败: {e}")
                logger.error(f"数据加载失败: {e}")
            finally:
                # 清理临时文件
                if os.path.exists(tmp_path):
                    os.unlink(tmp_path)
    
    def _handle_example_data(self):
        """处理示例数据"""
        example_options = {
            "鸢尾花数据集": "iris",
            "泰坦尼克号数据集": "titanic",
            "波士顿房价数据集": "boston",
            "糖尿病数据集": "diabetes",
            "葡萄酒数据集": "wine",
            "乳腺癌数据集": "breast_cancer"
        }
        
        selected_example = st.selectbox("选择示例数据集", list(example_options.keys()))
        
        if st.button("加载示例数据", type="primary"):
            with st.spinner("正在加载示例数据..."):
                try:
                    # 这里可以调用R API或使用本地数据
                    if example_options[selected_example] == "iris":
                        from sklearn.datasets import load_iris
                        data = load_iris()
                        df = pd.DataFrame(data.data, columns=data.feature_names)
                        df['target'] = data.target
                        
                    elif example_options[selected_example] == "titanic":
                        df = pd.read_csv("https://web.stanford.edu/class/archive/cs/cs109/cs109.1166/stuff/titanic.csv")
                        
                    elif example_options[selected_example] == "boston":
                        from sklearn.datasets import fetch_openml
                        boston = fetch_openml(name='boston', version=1)
                        df = pd.DataFrame(boston.data, columns=boston.feature_names)
                        df['target'] = boston.target
                        
                    elif example_options[selected_example] == "diabetes":
                        from sklearn.datasets import load_diabetes
                        diabetes = load_diabetes()
                        df = pd.DataFrame(diabetes.data, columns=diabetes.feature_names)
                        df['target'] = diabetes.target
                        
                    state.raw_data = df
                    state.data_loaded = True
                    
                    st.success(f"✅ 示例数据加载成功: {df.shape[0]} 行 × {df.shape[1]} 列")
                    
                    with st.expander("📋 数据预览", expanded=True):
                        st.dataframe(df.head(), use_container_width=True)
                    
                    if st.button("下一步: 数据探索", type="primary", use_container_width=True):
                        state.current_step = 2
                        st.rerun()
                        
                except Exception as e:
                    st.error(f"加载示例数据失败: {e}")
    
    def _handle_database_connection(self):
        """处理数据库连接"""
        st.info("数据库连接功能正在开发中...")
        
        # 数据库配置
        db_type = st.selectbox("数据库类型", ["PostgreSQL", "MySQL"])
        
        col1, col2 = st.columns(2)
        with col1:
            host = st.text_input("主机", "localhost")
            port = st.number_input("端口", value=5432 if db_type == "PostgreSQL" else 3306)
        with col2:
            database = st.text_input("数据库名", "st_db")
            username = st.text_input("用户名", "aikemi001")
            password = st.text_input("密码", type="snut3426")
        
        query = st.text_area("SQL查询", "SELECT * FROM table_name LIMIT 1000")
        
        if st.button("连接数据库", type="primary"):
            st.info("数据库连接成功")
    
    def _handle_api_source(self):
        """处理API数据源"""
        st.info("API数据源功能正在开发中...")
        
        api_url = st.text_input("API地址", "https://api.example.com/data")
        api_key = st.text_input("API密钥", type="password")
        
        if st.button("从API加载数据", type="primary"):
            st.info("API数据源功能将在后续版本中实现")
    
    def page_data_exploration(self):
        """数据探索页面"""
        st.markdown('<h2 class="section-title">🔍 数据探索</h2>', 
                   unsafe_allow_html=True)
        
        if not state.data_loaded:
            st.warning("请先加载数据")
            st.stop()
        
        df = state.raw_data
        
        # 探索选项卡
        tab1, tab2, tab3, tab4, tab5 = st.tabs([
            "📊 数据概览", 
            "📈 数据可视化", 
            "🔢 统计分析", 
            "🔍 数据质量", 
            "🎯 目标变量"
        ])
        
        with tab1:
            self._show_data_overview(df)
        with tab2:
            self._show_data_visualization(df)
        with tab3:
            self._show_statistical_analysis(df)
        with tab4:
            self._show_data_quality(df)
        with tab5:
            self._show_target_variable_selection(df)
    
    def _show_data_overview(self, df):
        """显示数据概览"""
        col1, col2, col3, col4 = st.columns(4)
        
        with col1:
            st.metric("总行数", df.shape[0])
        with col2:
            st.metric("总列数", df.shape[1])
        with col3:
            st.metric("内存使用", f"{df.memory_usage(deep=True).sum() / 1024 / 1024:.2f} MB")
        with col4:
            missing_total = df.isnull().sum().sum()
            st.metric("缺失值总数", missing_total)
        
        # 数据类型分布
        st.subheader("数据类型分布")
        dtype_counts = df.dtypes.value_counts()
        fig = px.pie(
            values=dtype_counts.values,
            names=dtype_counts.index.astype(str),
            title="数据类型分布"
        )
        st.plotly_chart(fig, use_container_width=True)
        
        # 列信息表格
        st.subheader("列信息")
        col_info = pd.DataFrame({
            '列名': df.columns,
            '数据类型': df.dtypes.values,
            '非空值': df.count().values,
            '缺失值': df.isnull().sum().values,
            '缺失率%': (df.isnull().sum() / len(df) * 100).round(2).values,
            '唯一值': df.nunique().values,
            '示例值': df.iloc[0].values
        })
        st.dataframe(col_info, use_container_width=True)
    
    def _show_data_visualization(self, df):
        """显示数据可视化"""
        # 选择可视化类型
        viz_type = st.selectbox(
            "选择可视化类型",
            ["分布图", "散点图", "箱线图", "小提琴图", "热力图", "相关性矩阵"]
        )
        
        if viz_type == "分布图":
            col = st.selectbox("选择列", df.select_dtypes(include=[np.number]).columns)
            if col:
                fig = px.histogram(df, x=col, nbins=50, title=f"{col}分布")
                st.plotly_chart(fig, use_container_width=True)
                
        elif viz_type == "散点图":
            col1 = st.selectbox("X轴", df.select_dtypes(include=[np.number]).columns)
            col2 = st.selectbox("Y轴", df.select_dtypes(include=[np.number]).columns)
            color_col = st.selectbox("颜色列(可选)", ["无"] + list(df.columns))
            
            if col1 and col2:
                if color_col != "无":
                    fig = px.scatter(df, x=col1, y=col2, color=color_col, 
                                    title=f"{col1} vs {col2}")
                else:
                    fig = px.scatter(df, x=col1, y=col2, title=f"{col1} vs {col2}")
                st.plotly_chart(fig, use_container_width=True)
                
        elif viz_type == "箱线图":
            numeric_cols = df.select_dtypes(include=[np.number]).columns
            selected_cols = st.multiselect("选择列", numeric_cols, default=list(numeric_cols[:3]))
            
            if selected_cols:
                fig = px.box(df[selected_cols], title="箱线图")
                st.plotly_chart(fig, use_container_width=True)
                
        elif viz_type == "热力图":
            numeric_cols = df.select_dtypes(include=[np.number]).columns
            if len(numeric_cols) > 1:
                corr_matrix = df[numeric_cols].corr()
                fig = px.imshow(corr_matrix, title="相关性热力图", 
                               color_continuous_scale="RdBu")
                st.plotly_chart(fig, use_container_width=True)
            else:
                st.warning("需要至少2个数值列来生成热力图")
    
    def _show_statistical_analysis(self, df):
        """显示统计分析"""
        st.subheader("描述性统计")
        st.dataframe(df.describe(), use_container_width=True)
        
        # 偏度和峰度
        st.subheader("分布特性")
        numeric_cols = df.select_dtypes(include=[np.number]).columns
        
        if len(numeric_cols) > 0:
            from scipy import stats
            
            dist_stats = []
            for col in numeric_cols:
                skewness = stats.skew(df[col].dropna())
                kurtosis = stats.kurtosis(df[col].dropna())
                dist_stats.append({
                    '列名': col,
                    '偏度': f"{skewness:.4f}",
                    '峰度': f"{kurtosis:.4f}",
                    '正态性(p值)': f"{stats.shapiro(df[col].dropna())[1]:.4f}"
                })
            
            dist_df = pd.DataFrame(dist_stats)
            st.dataframe(dist_df, use_container_width=True)
    
    def _show_data_quality(self, df):
        """显示数据质量分析"""
        # 缺失值分析
        st.subheader("缺失值分析")
        missing_df = pd.DataFrame({
            '列名': df.columns,
            '缺失值数量': df.isnull().sum().values,
            '缺失率%': (df.isnull().sum() / len(df) * 100).round(2).values
        }).sort_values('缺失率%', ascending=False)
        
        st.dataframe(missing_df, use_container_width=True)
        
        # 缺失值可视化
        if missing_df['缺失值数量'].sum() > 0:
            fig = px.bar(missing_df, x='列名', y='缺失率%', title='缺失值分布')
            st.plotly_chart(fig, use_container_width=True)
        
        # 重复值分析
        st.subheader("重复值分析")
        duplicate_count = df.duplicated().sum()
        st.metric("重复行数", duplicate_count)
        
        if duplicate_count > 0:
            st.warning(f"发现 {duplicate_count} 个重复行")
            if st.button("查看重复行"):
                st.dataframe(df[df.duplicated()], use_container_width=True)
        
        # 异常值检测
        st.subheader("异常值检测")
        numeric_cols = df.select_dtypes(include=[np.number]).columns
        
        if len(numeric_cols) > 0:
            outlier_method = st.selectbox("检测方法", ["IQR方法", "Z-score方法"])
            
            if st.button("检测异常值"):
                with st.spinner("正在检测异常值..."):
                    outliers_summary = {}
                    
                    for col in numeric_cols:
                        data = df[col].dropna()
                        
                        if outlier_method == "IQR方法":
                            Q1 = data.quantile(0.25)
                            Q3 = data.quantile(0.75)
                            IQR = Q3 - Q1
                            lower_bound = Q1 - 1.5 * IQR
                            upper_bound = Q3 + 1.5 * IQR
                            outliers = data[(data < lower_bound) | (data > upper_bound)]
                        else:  # Z-score方法
                            from scipy import stats
                            z_scores = np.abs(stats.zscore(data))
                            outliers = data[z_scores > 3]
                        
                        outliers_summary[col] = {
                            '异常值数量': len(outliers),
                            '异常值比例%': (len(outliers) / len(data) * 100),
                            '最小值': data.min(),
                            '最大值': data.max()
                        }
                    
                    outliers_df = pd.DataFrame(outliers_summary).T
                    st.dataframe(outliers_df, use_container_width=True)
    
    def _show_target_variable_selection(self, df):
        """显示目标变量选择"""
        st.subheader("目标变量设置")
        
        # 选择目标变量
        target_col = st.selectbox(
            "选择目标变量(要预测的列)",
            df.columns,
            help="这是模型要预测的变量"
        )
        
        # 检测问题类型
        if target_col:
            unique_values = df[target_col].nunique()
            
            if df[target_col].dtype in ['object', 'category'] or unique_values < 10:
                problem_type = "classification"
                st.info(f"检测为分类问题 (目标变量有 {unique_values} 个唯一值)")
            else:
                problem_type = "regression"
                st.info("检测为回归问题")
            
            # 目标变量分布
            st.subheader("目标变量分布")
            
            if problem_type == "classification":
                value_counts = df[target_col].value_counts()
                fig = px.pie(
                    values=value_counts.values,
                    names=value_counts.index,
                    title="目标变量类别分布"
                )
            else:
                fig = px.histogram(df, x=target_col, nbins=50, 
                                 title="目标变量分布")
            
            st.plotly_chart(fig, use_container_width=True)
            
            # 保存选择
            state.target_column = target_col
            state.problem_type = problem_type
            
            # 下一步按钮
            if st.button("下一步: 数据预处理", type="primary", use_container_width=True):
                state.current_step = 3
                st.rerun()
    
    def page_data_preprocessing(self):
        """数据预处理页面"""
        st.markdown('<h2 class="section-title">🧹 数据预处理</h2>', 
                   unsafe_allow_html=True)
        
        if not state.data_loaded or state.target_column is None:
            st.warning("请先加载数据并选择目标变量")
            st.stop()
        
        df = state.raw_data
        
        # 预处理配置
        st.subheader("预处理配置")
        
        col1, col2 = st.columns(2)
        
        with col1:
            st.markdown("## 缺失值处理")
            missing_strategy = st.selectbox(
                "处理策略",
                ["删除缺失行", "均值填充", "中位数填充", "众数填充", "插值法", "KNN填充"]
            )
            
            st.markdown("## 异常值处理")
            handle_outliers = st.checkbox("处理异常值", value=True)
            if handle_outliers:
                outlier_method = st.selectbox(
                    "异常值处理方法",
                    ["IQR过滤", "Z-score过滤", "保留异常值"]
                )
        
        with col2:
            st.markdown("## 特征工程")
            scale_features = st.checkbox("标准化特征", value=True)
            encode_categorical = st.checkbox("编码分类变量", value=True)
            
            st.markdown("## 特征选择")
            feature_selection = st.checkbox("启用特征选择", value=False)
            if feature_selection:
                selection_method = st.selectbox(
                    "选择方法",
                    ["相关性过滤", "方差过滤", "递归消除", "基于模型"]
                )
        
        # 数据分割配置
        st.subheader("数据分割")
        
        col1, col2, col3 = st.columns(3)
        with col1:
            test_size = st.slider("测试集比例", 0.1, 0.5, 0.2, 0.05)
        with col2:
            validation_size = st.slider("验证集比例", 0.0, 0.3, 0.1, 0.05)
        with col3:
            random_seed = st.number_input("随机种子", value=42)
        
        # 执行预处理
        if st.button("执行预处理", type="primary", use_container_width=True):
            with st.spinner("正在进行数据预处理..."):
                try:
                    # 这里应该调用R API进行预处理
                    # 暂时使用Python实现简单预处理
                    
                    from sklearn.model_selection import train_test_split
                    from sklearn.preprocessing import StandardScaler, LabelEncoder
                    
                    # 分离特征和目标
                    X = df.drop(columns=[state.target_column])
                    y = df[state.target_column]
                    
                    # 处理缺失值
                    if missing_strategy == "删除缺失行":
                        X = X.dropna()
                        y = y[X.index]
                    elif missing_strategy == "均值填充":
                        X = X.fillna(X.mean())
                    elif missing_strategy == "中位数填充":
                        X = X.fillna(X.median())
                    elif missing_strategy == "众数填充":
                        X = X.fillna(X.mode().iloc[0])
                    
                    # 编码分类变量
                    if encode_categorical:
                        categorical_cols = X.select_dtypes(include=['object', 'category']).columns
                        for col in categorical_cols:
                            le = LabelEncoder()
                            X[col] = le.fit_transform(X[col])
                        
                        # 编码目标变量（如果是分类问题）
                        if state.problem_type == "classification":
                            le_target = LabelEncoder()
                            y = le_target.fit_transform(y)
                    
                    # 标准化特征
                    if scale_features:
                        numeric_cols = X.select_dtypes(include=[np.number]).columns
                        scaler = StandardScaler()
                        X[numeric_cols] = scaler.fit_transform(X[numeric_cols])
                    
                    # 数据分割
                    X_train, X_test, y_train, y_test = train_test_split(
                        X, y, test_size=test_size, random_state=random_seed
                    )
                    
                    # 进一步分割验证集
                    if validation_size > 0:
                        val_ratio = validation_size / (1 - test_size)
                        X_train, X_val, y_train, y_val = train_test_split(
                            X_train, y_train, test_size=val_ratio, random_state=random_seed
                        )
                    
                    # 保存处理后的数据
                    state.processed_data = {
                        'X_train': X_train,
                        'X_test': X_test,
                        'y_train': y_train,
                        'y_test': y_test,
                        'feature_names': X.columns.tolist()
                    }
                    
                    if validation_size > 0:
                        state.processed_data.update({
                            'X_val': X_val,
                            'y_val': y_val
                        })
                    
                    st.success("✅ 数据预处理完成")
                    
                    # 显示处理结果
                    col1, col2, col3 = st.columns(3)
                    with col1:
                        st.metric("训练集", f"{len(X_train)} 样本")
                    with col2:
                        st.metric("测试集", f"{len(X_test)} 样本")
                    with col3:
                        if validation_size > 0:
                            st.metric("验证集", f"{len(X_val)} 样本")
                    
                    # 特征信息
                    with st.expander("📋 特征信息", expanded=False):
                        st.write(f"**特征数量**: {len(X.columns)}")
                        st.write(f"**特征列表**: {', '.join(X.columns.tolist()[:10])}")
                        if len(X.columns) > 10:
                            st.write(f"... 还有 {len(X.columns) - 10} 个特征")
                    
                    # 下一步按钮
                    if st.button("下一步: 模型训练", type="primary", use_container_width=True):
                        state.current_step = 4
                        st.rerun()
                        
                except Exception as e:
                    st.error(f"预处理失败: {e}")
                    logger.error(f"数据预处理失败: {e}")
    
    def page_model_training(self):
        """模型训练页面"""
        st.markdown('<h2 class="section-title">🤖 模型训练</h2>', 
                   unsafe_allow_html=True)
        
        if state.processed_data is None:
            st.warning("请先完成数据预处理")
            st.stop()
        
        # 模型选择
        st.subheader("选择算法")
        
        algorithm_options = {
            "逻辑回归": "logistic",
            "随机森林": "random_forest",
            "梯度提升树": "xgboost",
            "支持向量机": "svm",
            "神经网络": "neural_network",
            "决策树": "decision_tree",
            "K最近邻": "knn",
            "朴素贝叶斯": "naive_bayes"
        }
        
        selected_algorithms = st.multiselect(
            "选择要训练的算法（可多选）",
            list(algorithm_options.keys()),
            default=["逻辑回归", "随机森林", "梯度提升树"]
        )
        
        # 训练配置
        st.subheader("训练配置")
        
        col1, col2 = st.columns(2)
        
        with col1:
            cross_validation = st.checkbox("交叉验证", value=True)
            if cross_validation:
                cv_folds = st.slider("交叉验证折数", 3, 10, 5)
            
            early_stopping = st.checkbox("早停法", value=True)
            if early_stopping:
                patience = st.slider("耐心值", 5, 50, 10)
        
        with col2:
            hyperparameter_tuning = st.checkbox("超参数调优", value=True)
            if hyperparameter_tuning:
                tuning_method = st.selectbox(
                    "调优方法",
                    ["网格搜索", "随机搜索", "贝叶斯优化"]
                )
            
            ensemble_learning = st.checkbox("集成学习", value=False)
            if ensemble_learning:
                ensemble_method = st.selectbox(
                    "集成方法",
                    ["投票法", "堆叠法", "Bagging", "Boosting"]
                )
        
        # 高级参数配置
        with st.expander("🔧 高级参数配置"):
            tab1, tab2, tab3 = st.tabs(["随机森林", "XGBoost", "神经网络"])
            
            with tab1:
                rf_n_estimators = st.slider("树的数量", 10, 500, 100)
                rf_max_depth = st.slider("最大深度", 3, 20, 10)
                rf_min_samples_split = st.slider("最小分裂样本数", 2, 20, 2)
            
            with tab2:
                xgb_n_estimators = st.slider("树的数量", 10, 500, 100)
                xgb_learning_rate = st.slider("学习率", 0.01, 0.3, 0.1, 0.01)
                xgb_max_depth = st.slider("最大深度", 3, 20, 6)
            
            with tab3:
                nn_hidden_layers = st.slider("隐藏层层数", 1, 5, 2)
                nn_neurons_per_layer = st.slider("每层神经元数", 10, 200, 64)
                nn_activation = st.selectbox("激活函数", ["relu", "sigmoid", "tanh"])
        
        # 开始训练
        if st.button("开始训练", type="primary", use_container_width=True):
            with st.spinner("正在训练模型，这可能需要一些时间..."):
                try:
                    # 构建训练配置
                    train_config = {
                        "algorithms": [algorithm_options[alg] for alg in selected_algorithms],
                        "problem_type": state.problem_type,
                        "cross_validation": cross_validation,
                        "cv_folds": cv_folds if cross_validation else None,
                        "hyperparameter_tuning": hyperparameter_tuning,
                        "tuning_method": tuning_method if hyperparameter_tuning else None,
                        "early_stopping": early_stopping,
                        "patience": patience if early_stopping else None,
                        "ensemble_learning": ensemble_learning,
                        "ensemble_method": ensemble_method if ensemble_learning else None,
                        "random_forest": {
                            "n_estimators": rf_n_estimators,
                            "max_depth": rf_max_depth,
                            "min_samples_split": rf_min_samples_split
                        },
                        "xgboost": {
                            "n_estimators": xgb_n_estimators,
                            "learning_rate": xgb_learning_rate,
                            "max_depth": xgb_max_depth
                        },
                        "neural_network": {
                            "hidden_layers": nn_hidden_layers,
                            "neurons_per_layer": nn_neurons_per_layer,
                            "activation": nn_activation
                        }
                    }
                    
                    # 调用R API进行训练
                    client = APIClient()
                    
                    # 准备训练数据
                    train_data = {
                        "X_train": state.processed_data["X_train"].to_dict(orient="list"),
                        "y_train": state.processed_data["y_train"].tolist(),
                        "X_test": state.processed_data["X_test"].to_dict(orient="list"),
                        "y_test": state.processed_data["y_test"].tolist(),
                        "feature_names": state.processed_data["feature_names"],
                        "config": train_config
                    }
                    
                    response = client.sync_request(
                        "POST",
                        "train_models",
                        json=train_data
                    )
                    
                    if response.get("success", False):
                        state.training_result = response
                        state.models_trained = True
                        
                        st.success("✅ 模型训练完成")
                        
                        # 显示训练结果
                        self._show_training_results(response)
                        
                        # 下一步按钮
                        if st.button("下一步: 模型评估", type="primary", use_container_width=True):
                            state.current_step = 5
                            st.rerun()
                    else:
                        st.error(f"训练失败: {response.get('error', '未知错误')}")
                        
                except Exception as e:
                    st.error(f"训练错误: {e}")
                    logger.error(f"模型训练失败: {e}")
    
    def _show_training_results(self, result):
        """显示训练结果"""
        # 模型性能对比
        st.subheader("模型性能对比")
        
        if "model_performance" in result:
            performance_data = []
            for model_name, perf in result["model_performance"].items():
                performance_data.append({
                    "模型": model_name,
                    "准确率": perf.get("accuracy", 0),
                    "精确率": perf.get("precision", 0),
                    "召回率": perf.get("recall", 0),
                    "F1分数": perf.get("f1", 0),
                    "AUC": perf.get("auc", 0)
                })
            
            perf_df = pd.DataFrame(performance_data)
            st.dataframe(perf_df, use_container_width=True)
            
            # 性能对比图
            fig = px.bar(
                perf_df.melt(id_vars=["模型"], var_name="指标", value_name="值"),
                x="模型", y="值", color="指标", barmode="group",
                title="模型性能对比"
            )
            st.plotly_chart(fig, use_container_width=True)
        
        # 最佳模型
        if "best_model" in result:
            best_model = result["best_model"]
            st.subheader("最佳模型")
            
            col1, col2, col3, col4 = st.columns(4)
            with col1:
                st.metric("模型名称", best_model.get("name", "未知"))
            with col2:
                st.metric("算法", best_model.get("algorithm", "未知"))
            with col3:
                st.metric("准确率", f"{best_model.get('accuracy', 0):.4f}")
            with col4:
                st.metric("训练时间", f"{best_model.get('training_time', 0):.2f}秒")
            
            # 保存最佳模型
            state.selected_model = best_model
    
    def page_model_evaluation(self):
        """模型评估页面"""
        st.markdown('<h2 class="section-title">📈 模型评估</h2>', 
                   unsafe_allow_html=True)
        
        if not state.models_trained:
            st.warning("请先训练模型")
            st.stop()
        
        # 选择要评估的模型
        model_options = list(state.training_result.get("model_performance", {}).keys())
        if not model_options:
            st.error("没有可用的模型")
            st.stop()
        
        selected_model = st.selectbox(
            "选择要评估的模型",
            model_options,
            index=0
        )
        
        # 评估指标
        st.subheader("评估指标")
        
        # 获取模型性能
        model_perf = state.training_result["model_performance"].get(selected_model, {})
        
        col1, col2, col3, col4 = st.columns(4)
        with col1:
            st.metric("准确率", f"{model_perf.get('accuracy', 0):.4f}")
        with col2:
            st.metric("精确率", f"{model_perf.get('precision', 0):.4f}")
        with col3:
            st.metric("召回率", f"{model_perf.get('recall', 0):.4f}")
        with col4:
            st.metric("F1分数", f"{model_perf.get('f1', 0):.4f}")
        
        # 可视化
        st.subheader("模型可视化")
        
        viz_type = st.selectbox(
            "选择可视化类型",
            ["混淆矩阵", "ROC曲线", "PR曲线", "特征重要性", "学习曲线"]
        )
        
        if st.button("生成可视化", type="primary"):
            with st.spinner("正在生成可视化..."):
                try:
                    # 调用R API获取可视化
                    client = APIClient()
                    
                    viz_request = {
                        "model_name": selected_model,
                        "viz_type": viz_type,
                        "test_data": {
                            "X": state.processed_data["X_test"].to_dict(orient="list"),
                            "y": state.processed_data["y_test"].tolist()
                        }
                    }
                    
                    response = client.sync_request(
                        "POST",
                        "evaluate_model",
                        json=viz_request
                    )
                    
                    if response.get("success", False):
                        state.evaluation_result = response
                        
                        # 显示可视化结果
                        self._show_evaluation_visualizations(response, viz_type)
                    else:
                        st.error(f"获取可视化失败: {response.get('error', '未知错误')}")
                        
                except Exception as e:
                    st.error(f"评估错误: {e}")
        
        # 模型解释
        st.subheader("模型解释")
        
        if st.button("解释模型", type="secondary"):
            with st.spinner("正在解释模型..."):
                try:
                    client = APIClient()
                    
                    explain_request = {
                        "model_name": selected_model,
                        "sample_data": state.processed_data["X_test"].iloc[0].to_dict()
                    }
                    
                    response = client.sync_request(
                        "POST",
                        "explain_model",
                        json=explain_request
                    )
                    
                    if response.get("success", False):
                        explanation = response.get("explanation", {})
                        
                        # 显示特征重要性
                        if "feature_importance" in explanation:
                            st.markdown("## 特征重要性")
                            importance_df = pd.DataFrame(
                                explanation["feature_importance"].items(),
                                columns=["特征", "重要性"]
                            ).sort_values("重要性", ascending=False)
                            
                            st.dataframe(importance_df, use_container_width=True)
                            
                            # 可视化
                            fig = px.bar(
                                importance_df.head(10),
                                x="重要性", y="特征", orientation='h',
                                title="Top 10 重要特征"
                            )
                            st.plotly_chart(fig, use_container_width=True)
                        
                        # 显示SHAP值
                        if "shap_values" in explanation:
                            st.markdown("## SHAP值分析")
                            # 这里可以添加SHAP可视化
                    else:
                        st.warning("模型解释功能不可用")
                        
                except Exception as e:
                    st.warning(f"模型解释失败: {e}")
        
        # 下一步按钮
        if st.button("下一步: 模型部署", type="primary", use_container_width=True):
            state.current_step = 6
            st.rerun()
    
    def _show_evaluation_visualizations(self, result, viz_type):
        """显示评估可视化"""
        if viz_type == "混淆矩阵" and "confusion_matrix" in result:
            cm_data = result["confusion_matrix"]
            # 这里可以显示混淆矩阵
            st.write("混淆矩阵:")
            st.write(cm_data)
            
        elif viz_type == "ROC曲线" and "roc_curve" in result:
            roc_data = result["roc_curve"]
            # 这里可以显示ROC曲线
            st.write("ROC曲线数据:")
            st.write(roc_data)
            
        elif viz_type == "特征重要性" and "feature_importance" in result:
            importance_data = result["feature_importance"]
            importance_df = pd.DataFrame(
                importance_data.items(),
                columns=["特征", "重要性"]
            ).sort_values("重要性", ascending=False)
            
            st.dataframe(importance_df, use_container_width=True)
            
            fig = px.bar(
                importance_df.head(15),
                x="重要性", y="特征", orientation='h',
                title="特征重要性"
            )
            st.plotly_chart(fig, use_container_width=True)
    
    def page_model_deployment(self):
        """模型部署页面"""
        st.markdown('<h2 class="section-title">🚀 模型部署</h2>', 
                   unsafe_allow_html=True)
        
        if not state.models_trained:
            st.warning("请先训练模型")
            st.stop()
        
        # 部署选项
        st.subheader("部署方式")
        
        deployment_type = st.radio(
            "选择部署方式",
            ["REST API", "Docker容器", "模型文件", "云服务"],
            horizontal=True
        )
        
        # 模型选择
        model_options = list(state.training_result.get("model_performance", {}).keys())
        selected_model = st.selectbox(
            "选择要部署的模型",
            model_options
        )
        
        # 部署配置
        if deployment_type == "REST API":
            self._show_api_deployment_config(selected_model)
        elif deployment_type == "Docker容器":
            self._show_docker_deployment_config(selected_model)
        elif deployment_type == "模型文件":
            self._show_model_file_config(selected_model)
        else:
            self._show_cloud_deployment_config(selected_model)
    
    def _show_api_deployment_config(self, model_name):
        """显示API部署配置"""
        st.subheader("REST API部署配置")
        
        col1, col2 = st.columns(2)
        
        with col1:
            api_port = st.number_input("API端口", 8000, 9000, 8080)
            api_host = st.text_input("绑定地址", "127.0.0.1")
            enable_docs = st.checkbox("启用API文档", value=True)
        
        with col2:
            rate_limit = st.number_input("请求限制(次/分钟)", 10, 1000, 100)
            enable_auth = st.checkbox("启用认证", value=False)
            if enable_auth:
                auth_method = st.selectbox("认证方式", ["API密钥", "JWT", "OAuth2"])
        
        # 部署按钮
        if st.button("部署为REST API", type="primary", use_container_width=True):
            with st.spinner("正在部署API服务..."):
                try:
                    client = APIClient()
                    
                    deploy_config = {
                        "model_name": model_name,
                        "deployment_type": "api",
                        "config": {
                            "port": api_port,
                            "host": api_host,
                            "enable_docs": enable_docs,
                            "rate_limit": rate_limit,
                            "enable_auth": enable_auth,
                            "auth_method": auth_method if enable_auth else None
                        }
                    }
                    
                    response = client.sync_request(
                        "POST",
                        "deploy_model",
                        json=deploy_config
                    )
                    
                    if response.get("success", False):
                        st.success("✅ API部署成功")
                        
                        # 显示API信息
                        api_info = response.get("api_info", {})
                        
                        col1, col2 = st.columns(2)
                        with col1:
                            st.info(f"**API地址**: http://{api_host}:{api_port}")
                            st.info(f"**模型端点**: /predict")
                        with col2:
                            if enable_docs:
                                st.info(f"**文档地址**: http://{api_host}:{api_port}/__swagger__/")
                        
                        # 测试API
                        if st.button("测试API", type="secondary"):
                            self._test_api_endpoint(api_host, api_port, model_name)
                    else:
                        st.error(f"部署失败: {response.get('error', '未知错误')}")
                        
                except Exception as e:
                    st.error(f"部署错误: {e}")
    
    def _show_docker_deployment_config(self, model_name):
        """显示Docker部署配置"""
        st.subheader("Docker容器部署配置")
        
        col1, col2 = st.columns(2)
        
        with col1:
            docker_image = st.text_input("镜像名称", f"mlweb-{model_name}")
            docker_tag = st.text_input("镜像标签", "latest")
            expose_port = st.number_input("暴露端口", 8000, 9000, 8080)
        
        with col2:
            enable_gpu = st.checkbox("启用GPU支持", value=False)
            resource_limit = st.checkbox("资源限制", value=False)
            if resource_limit:
                cpu_limit = st.text_input("CPU限制", "2")
                memory_limit = st.text_input("内存限制", "4g")
        
        # 生成Dockerfile
        if st.button("生成Dockerfile", type="secondary"):
            dockerfile = f"""FROM rocker/r-ver:4.3.0

# 安装系统依赖
RUN apt-get update && apt-get install -y \\
    python3 \\
    python3-pip \\
    libcurl4-openssl-dev \\
    libssl-dev \\
    libxml2-dev \\
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# 复制模型文件
COPY models/{model_name}.rds ./model.rds
COPY r_scripts/ ./r_scripts/

# 安装R依赖
RUN Rscript -e "install.packages(c('plumber', 'caret', 'jsonlite'), repos='https://cloud.r-project.org')"

# 创建API文件
COPY plumber_api.R .

# 暴露端口
EXPOSE {expose_port}

# 启动命令
CMD ["Rscript", "-e", "library(plumber); pr <- plumb('plumber_api.R'); pr$run(host='127.0.0.1', port={expose_port})"]"""
            
            st.code(dockerfile, language="dockerfile")
            
            # 下载Dockerfile
            b64 = base64.b64encode(dockerfile.encode()).decode()
            href = f'<a href="data:text/plain;base64,{b64}" download="Dockerfile">下载Dockerfile</a>'
            st.markdown(href, unsafe_allow_html=True)
        
        # 构建镜像
        if st.button("构建Docker镜像", type="primary", use_container_width=True):
            st.info("请在终端执行以下命令:")
            st.code(f"docker build -t {docker_image}:{docker_tag} .")
            st.code(f"docker run -p {expose_port}:{expose_port} {docker_image}:{docker_tag}")
    
    def _show_model_file_config(self, model_name):
        """显示模型文件配置"""
        st.subheader("模型文件导出")
        
        export_format = st.radio(
            "导出格式",
            [".rds (R格式)", ".pmml (通用格式)", ".onnx (深度学习)", ".pkl (Python格式)"],
            horizontal=True
        )
        
        include_preprocessor = st.checkbox("包含预处理管道", value=True)
        include_documentation = st.checkbox("包含使用文档", value=True)
        
        if st.button("导出模型文件", type="primary", use_container_width=True):
            with st.spinner("正在导出模型..."):
                try:
                    client = APIClient()
                    
                    export_config = {
                        "model_name": model_name,
                        "export_format": export_format.split(" ")[0].lstrip("."),
                        "include_preprocessor": include_preprocessor,
                        "include_documentation": include_documentation
                    }
                    
                    response = client.sync_request(
                        "POST",
                        "export_model",
                        json=export_config
                    )
                    
                    if response.get("success", False):
                        st.success("✅ 模型导出成功")
                        
                        # 提供下载
                        if "file_content" in response:
                            file_format = export_format.split(" ")[0].lstrip(".")
                            file_name = f"{model_name}.{file_format}"
                            
                            if file_format == "rds":
                                mime_type = "application/octet-stream"
                            elif file_format == "pmml":
                                mime_type = "application/xml"
                            elif file_format == "onnx":
                                mime_type = "application/octet-stream"
                            else:
                                mime_type = "application/octet-stream"
                            
                            b64 = base64.b64encode(response["file_content"].encode()).decode()
                            href = f'<a href="data:{mime_type};base64,{b64}" download="{file_name}">下载模型文件</a>'
                            st.markdown(href, unsafe_allow_html=True)
                    else:
                        st.error(f"导出失败: {response.get('error', '未知错误')}")
                        
                except Exception as e:
                    st.error(f"导出错误: {e}")
    
    def _show_cloud_deployment_config(self, model_name):
        """显示云服务部署配置"""
        st.subheader("云服务部署")
        
        cloud_provider = st.selectbox(
            "云服务商",
            ["AWS SageMaker", "Google AI Platform", "Azure ML", "阿里云PAI", "华为云ModelArts"]
        )
        
        if cloud_provider == "AWS SageMaker":
            st.text_input("SageMaker端点名称", f"mlweb-{model_name}")
            st.text_input("IAM角色ARN", "arn:aws:iam::123456789012:role/SageMakerRole")
            st.text_input("S3存储桶", "mlweb-models")
            
        elif cloud_provider == "Google AI Platform":
            st.text_input("项目ID", "your-project-id")
            st.text_input("模型名称", f"mlweb_{model_name}")
            st.selectbox("机器类型", ["n1-standard-4", "n1-highmem-8", "n1-highcpu-16"])
            
        elif cloud_provider == "Azure ML":
            st.text_input("工作区名称", "mlweb-workspace")
            st.text_input("模型名称", f"mlweb-{model_name}")
            st.selectbox("计算目标", ["local", "amlcompute", "aks"])
        
        st.warning("云服务部署功能需要相应的云平台账户和配置")
        
        if st.button("部署到云平台", type="primary", disabled=True):
            st.info("此功能正在开发中")
    
    def _test_api_endpoint(self, host, port, model_name):
        """测试API端点"""
        test_url = f"http://{host}:{port}/health"
        
        try:
            response = requests.get(test_url, timeout=5)
            if response.status_code == 200:
                st.success("✅ API健康检查通过")
                
                # 测试预测端点
                if state.processed_data is not None:
                    sample_data = state.processed_data["X_test"].iloc[0].to_dict()
                    
                    predict_url = f"http://{host}:{port}/predict"
                    predict_response = requests.post(
                        predict_url,
                        json=sample_data,
                        timeout=10
                    )
                    
                    if predict_response.status_code == 200:
                        st.success("✅ 预测端点测试通过")
                        result = predict_response.json()
                        st.write("预测结果:", result)
                    else:
                        st.error(f"❌ 预测端点测试失败: {predict_response.status_code}")
            else:
                st.error(f"❌ API健康检查失败: {response.status_code}")
                
        except Exception as e:
            st.error(f"❌ API测试失败: {e}")
    
    def page_realtime_prediction(self):
        """实时预测页面"""
        st.markdown('<h2 class="section-title">⚡ 实时预测</h2>', 
                   unsafe_allow_html=True)
        
        if not state.models_trained:
            st.warning("请先训练模型")
            st.stop()
        
        # 预测方式选择
        predict_mode = st.radio(
            "预测方式",
            ["单样本预测", "批量预测", "文件预测", "API调用"],
            horizontal=True
        )
        
        if predict_mode == "单样本预测":
            self._show_single_prediction()
        elif predict_mode == "批量预测":
            self._show_batch_prediction()
        elif predict_mode == "文件预测":
            self._show_file_prediction()
        else:
            self._show_api_prediction()
    
    def _show_single_prediction(self):
        """显示单样本预测"""
        st.subheader("单样本预测")
        
        if state.processed_data is None:
            st.warning("请先完成数据预处理")
            st.stop()
        
        # 获取特征列表
        feature_names = state.processed_data["feature_names"]
        
        # 动态生成输入表单
        st.markdown("## 输入特征值")
        
        inputs = {}
        cols = st.columns(3)
        
        for i, feature in enumerate(feature_names):
            with cols[i % 3]:
                # 获取特征统计信息用于输入范围
                if state.raw_data is not None and feature in state.raw_data.columns:
                    col_data = state.raw_data[feature]
                    if pd.api.types.is_numeric_dtype(col_data):
                        min_val = float(col_data.min())
                        max_val = float(col_data.max())
                        mean_val = float(col_data.mean())
                        
                        inputs[feature] = st.number_input(
                            feature,
                            min_value=min_val,
                            max_value=max_val,
                            value=mean_val,
                            help=f"范围: [{min_val:.2f}, {max_val:.2f}]"
                        )
                    else:
                        unique_vals = col_data.unique()[:10]  # 限制显示前10个值
                        inputs[feature] = st.selectbox(feature, unique_vals)
                else:
                    inputs[feature] = st.number_input(feature, value=0.0)
        
        # 选择模型
        model_options = list(state.training_result.get("model_performance", {}).keys())
        selected_model = st.selectbox("选择预测模型", model_options)
        
        # 预测按钮
        if st.button("进行预测", type="primary", use_container_width=True):
            with st.spinner("正在预测..."):
                try:
                    client = APIClient()
                    
                    prediction_request = {
                        "model_name": selected_model,
                        "input_data": inputs
                    }
                    
                    response = client.sync_request(
                        "POST",
                        "predict",
                        json=prediction_request
                    )
                    
                    if response.get("success", False):
                        result = response.get("result", {})
                        
                        st.success("✅ 预测完成")
                        
                        # 显示结果
                        col1, col2, col3 = st.columns(3)
                        with col1:
                            st.metric("预测值", f"{result.get('prediction', 'N/A')}")
                        with col2:
                            if "probability" in result:
                                st.metric("置信度", f"{result['probability']:.2%}")
                        with col3:
                            st.metric("响应时间", f"{result.get('response_time', 0):.3f}秒")
                        
                        # 详细信息
                        with st.expander("📋 预测详情", expanded=False):
                            st.json(result)
                        
                        # 预测解释
                        if st.button("解释预测结果", type="secondary"):
                            self._explain_prediction(selected_model, inputs)
                    else:
                        st.error(f"预测失败: {response.get('error', '未知错误')}")
                        
                except Exception as e:
                    st.error(f"预测错误: {e}")
    
    def _show_batch_prediction(self):
        """显示批量预测"""
        st.subheader("批量预测")
        
        # 输入方式选择
        input_method = st.radio(
            "输入方式",
            ["手动输入", "上传文件", "从数据库"],
            horizontal=True
        )
        
        if input_method == "手动输入":
            # JSON输入
            default_json = '[{"feature1": 5.1, "feature2": 3.5}, {"feature1": 6.2, "feature2": 3.4}]'
            input_json = st.text_area(
                "输入JSON格式数据",
                value=default_json,
                height=200
            )
            
            try:
                data = json.loads(input_json)
                st.success(f"✅ 解析成功，共 {len(data)} 条记录")
                
            except json.JSONDecodeError as e:
                st.error(f"❌ JSON格式错误: {e}")
                data = None
                
        elif input_method == "上传文件":
            uploaded_file = st.file_uploader(
                "选择数据文件",
                type=["csv", "json", "xlsx"],
                help="支持CSV、JSON、Excel格式"
            )
            
            if uploaded_file is not None:
                try:
                    if uploaded_file.name.endswith('.csv'):
                        data = pd.read_csv(uploaded_file)
                    elif uploaded_file.name.endswith('.json'):
                        data = pd.read_json(uploaded_file)
                    elif uploaded_file.name.endswith(('.xlsx', '.xls')):
                        data = pd.read_excel(uploaded_file)
                    
                    st.success(f"✅ 文件加载成功: {len(data)} 条记录")
                    st.dataframe(data.head(), use_container_width=True)
                    
                except Exception as e:
                    st.error(f"❌ 文件加载失败: {e}")
                    data = None
            else:
                data = None
                
        else:  # 从数据库
            st.info("数据库输入功能正在开发中")
            data = None
        
        # 选择模型
        if data is not None:
            model_options = list(state.training_result.get("model_performance", {}).keys())
            selected_model = st.selectbox("选择预测模型", model_options)
            
            if st.button("批量预测", type="primary", use_container_width=True):
                with st.spinner("正在进行批量预测..."):
                    try:
                        client = APIClient()
                        
                        # 转换数据格式
                        if isinstance(data, pd.DataFrame):
                            batch_data = data.to_dict(orient="records")
                        else:
                            batch_data = data
                        
                        batch_request = {
                            "model_name": selected_model,
                            "batch_data": batch_data
                        }
                        
                        response = client.sync_request(
                            "POST",
                            "batch_predict",
                            json=batch_request
                        )
                        
                        if response.get("success", False):
                            result = response.get("result", {})
                            
                            st.success(f"✅ 批量预测完成，共 {result.get('count', 0)} 条记录")
                            
                            # 显示统计信息
                            col1, col2, col3 = st.columns(3)
                            with col1:
                                st.metric("总耗时", f"{result.get('total_time', 0):.3f}秒")
                            with col2:
                                st.metric("平均耗时", 
                                         f"{result.get('avg_time_per_prediction', 0):.3f}秒/样本")
                            with col3:
                                predictions = result.get('predictions', [])
                                if predictions:
                                    st.metric("平均预测值", f"{np.mean(predictions):.4f}")
                            
                            # 显示预测结果
                            predictions_df = pd.DataFrame({
                                "序号": range(1, len(predictions) + 1),
                                "预测值": predictions
                            })
                            
                            if "probabilities" in result:
                                predictions_df["置信度"] = result["probabilities"]
                            
                            st.dataframe(predictions_df, use_container_width=True)
                            
                            # 预测分布
                            if predictions:
                                fig = px.histogram(predictions_df, x="预测值", 
                                                 nbins=20, title="预测值分布")
                                st.plotly_chart(fig, use_container_width=True)
                            
                            # 下载结果
                            csv = predictions_df.to_csv(index=False)
                            st.download_button(
                                label="下载预测结果",
                                data=csv,
                                file_name="batch_predictions.csv",
                                mime="text/csv"
                            )
                        else:
                            st.error(f"批量预测失败: {response.get('error', '未知错误')}")
                            
                    except Exception as e:
                        st.error(f"批量预测错误: {e}")
    
    def _show_file_prediction(self):
        """显示文件预测"""
        st.subheader("文件预测")
        
        uploaded_file = st.file_uploader(
            "上传数据文件",
            type=["csv", "xlsx", "json"],
            help="文件应包含与训练数据相同的特征列"
        )
        
        if uploaded_file is not None:
            # 预览文件
            try:
                if uploaded_file.name.endswith('.csv'):
                    df = pd.read_csv(uploaded_file)
                elif uploaded_file.name.endswith('.json'):
                    df = pd.read_json(uploaded_file)
                elif uploaded_file.name.endswith(('.xlsx', '.xls')):
                    df = pd.read_excel(uploaded_file)
                
                st.success(f"✅ 文件加载成功: {df.shape[0]} 行 × {df.shape[1]} 列")
                
                with st.expander("📋 数据预览", expanded=True):
                    st.dataframe(df.head(), use_container_width=True)
                
                # 选择模型
                model_options = list(state.training_result.get("model_performance", {}).keys())
                selected_model = st.selectbox("选择预测模型", model_options)
                
                if st.button("执行文件预测", type="primary", use_container_width=True):
                    # 保存临时文件
                    with tempfile.NamedTemporaryFile(delete=False, suffix=".csv") as tmp_file:
                        df.to_csv(tmp_file.name, index=False)
                        tmp_path = tmp_file.name
                    
                    with st.spinner("正在进行文件预测..."):
                        try:
                            client = APIClient()
                            
                            file_request = {
                                "model_name": selected_model,
                                "file_path": tmp_path
                            }
                            
                            response = client.sync_request(
                                "POST",
                                "file_predict",
                                json=file_request
                            )
                            
                            if response.get("success", False):
                                result = response.get("result", {})
                                
                                st.success(f"✅ 文件预测完成，共 {result.get('count', 0)} 条记录")
                                
                                # 合并预测结果
                                predictions = result.get('predictions', [])
                                result_df = df.copy()
                                result_df['预测值'] = predictions
                                
                                if "probabilities" in result:
                                    result_df['置信度'] = result['probabilities']
                                
                                st.dataframe(result_df, use_container_width=True)
                                
                                # 下载完整结果
                                csv = result_df.to_csv(index=False)
                                st.download_button(
                                    label="下载完整结果",
                                    data=csv,
                                    file_name="file_predictions.csv",
                                    mime="text/csv"
                                )
                            else:
                                st.error(f"文件预测失败: {response.get('error', '未知错误')}")
                                
                        except Exception as e:
                            st.error(f"文件预测错误: {e}")
                        finally:
                            # 清理临时文件
                            if os.path.exists(tmp_path):
                                os.unlink(tmp_path)
                                
            except Exception as e:
                st.error(f"文件加载失败: {e}")
    
    def _show_api_prediction(self):
        """显示API调用预测"""
        st.subheader("API调用")
        
        st.info("""
        ## 使用API进行预测
        
        您可以使用以下端点通过HTTP请求进行预测:
        
        **健康检查**
        ```bash
        GET /health
        ```
        
        **单样本预测**
        ```bash
        POST /predict
        Content-Type: application/json
        
        {
          "model_name": "model_name",
          "input_data": {
            "feature1": 5.1,
            "feature2": 3.5,
            ...
          }
        }
        ```
        
        **批量预测**
        ```bash
        POST /batch_predict
        Content-Type: application/json
        
        {
          "model_name": "model_name",
          "batch_data": [
            {"feature1": 5.1, "feature2": 3.5},
            {"feature1": 6.2, "feature2": 3.4},
            ...
          ]
        }
        ```
        
        **文件预测**
        ```bash
        POST /file_predict
        Content-Type: application/json
        
        {
          "model_name": "model_name",
          "file_path": "/path/to/data.csv"
        }
        ```
        """)
        
        # API测试
        st.subheader("API测试")
        
        api_url = st.text_input("API地址", config.API_BASE_URL)
        endpoint = st.selectbox("测试端点", ["/health", "/predict", "/models"])
        
        if endpoint == "/predict" and state.processed_data is not None:
            sample_data = state.processed_data["X_test"].iloc[0].to_dict()
            st.code(f"""
            curl -X POST {api_url}/predict \\
                 -H "Content-Type: application/json" \\
                 -d '{json.dumps({"input_data": sample_data}, indent=2)}'
            """)
        else:
            st.code(f"curl {api_url}{endpoint}")
    
    def _explain_prediction(self, model_name, input_data):
        """解释单个预测"""
        try:
            client = APIClient()
            
            explain_request = {
                "model_name": model_name,
                "input_data": input_data
            }
            
            response = client.sync_request(
                "POST",
                "explain_prediction",
                json=explain_request
            )
            
            if response.get("success", False):
                explanation = response.get("explanation", {})
                
                st.subheader("预测解释")
                
                # 特征贡献
                if "feature_contributions" in explanation:
                    contrib_data = explanation["feature_contributions"]
                    contrib_df = pd.DataFrame(
                        contrib_data.items(),
                        columns=["特征", "贡献度"]
                    ).sort_values("贡献度", ascending=False)
                    
                    st.dataframe(contrib_df, use_container_width=True)
                    
                    # 可视化
                    fig = px.bar(
                        contrib_df,
                        x="贡献度", y="特征", orientation='h',
                        title="特征贡献度"
                    )
                    st.plotly_chart(fig, use_container_width=True)
                
                # 决策边界分析
                if "decision_boundary" in explanation:
                    st.markdown("## 决策边界分析")
                    # 这里可以显示决策边界可视化
                    
            else:
                st.warning("预测解释功能不可用")
                
        except Exception as e:
            st.warning(f"预测解释失败: {e}")

# 主应用
def main():
    """主应用入口"""
    
    # 初始化页面控制器
    controller = PageController()
    
    # 检查API连接
    if state.last_api_check is None or time.time() - state.last_api_check > 30:
        client = APIClient()
        status = client.check_health()
        state.api_status = status
        state.api_available = (status.get("status") == "healthy")
        state.last_api_check = time.time()
    
    # 渲染页面
    controller.render()
    
    # 性能监控
    if st.sidebar.checkbox("显示性能监控", value=False):
        with st.sidebar.expander("📊 性能监控", expanded=True):
            col1, col2 = st.columns(2)
            with col1:
                st.metric("内存使用", f"{psutil.Process().memory_info().rss / 1024 / 1024:.1f} MB")
            with col2:
                st.metric("CPU使用", f"{psutil.cpu_percent()}%")
            
            # API统计
            if state.api_available:
                client = APIClient()
                stats = client.get_stats()
                st.write("**API统计**:")
                st.write(f"- 总请求: {stats['total_requests']}")
                st.write(f"- 成功率: {stats['success_rate']:.1%}")

if __name__ == "__main__":
    # 导入psutil用于监控
    try:
        import psutil
    except ImportError:
        st.warning("安装psutil以启用性能监控: pip install psutil")
        psutil = None
    
    main()