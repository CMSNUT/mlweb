@echo off
chcp 65001 >nul
:: 关闭批处理自身的命令回显, 解决中文乱码

:: Conda安装根路径(必填, 替换为你的Anaconda/Miniconda路径)
set "CONDA_PATH=D:\anaconda3"
:: 项目根路径(脚本所在目录, 无需修改, 自动识别)
set "PROJECT_DIR=%~dp0"

:: R服务配置(无conda, 指定Rscript路径)
set "R_452_NAME=r452"                       :: R服务名称(用于显示)
set "R_452_PATH=%PROJECT_DIR%" :: R服务脚本目录
set "R_452_SCRIPT=api.R"                    :: R服务启动脚本
set "RSCRIPT_452_PATH=D:\R\R-4.5.2\bin\x64\Rscript.exe" :: Rscript路径

set "PY_310_ENV=py310"                     :: Python子服务环境名
:: set "PY_310_SCRIPT=app.py"                 :: Python子服务启动脚本

@REM echo 启动R服务(%R_452_NAME%)...
@REM start "R452-Service (%R_452_NAME%)" cmd /k "cd /d "%R_452_PATH%" && "%RSCRIPT_452_PATH%" %R_452_SCRIPT% && pause"

echo 启动Python主后端(%PY_MAIN_ENV%)...
start "Python-Main" cmd /k "echo 正在启动Python服务... && echo 激活conda环境... && call "%CONDA_PATH%\Scripts\activate.bat" %PY_310_ENV%  && cd /d backend && streamlit run main.py && pause"

:: streamlit run app.py