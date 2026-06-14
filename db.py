import os
import pandas as pd
from dotenv import load_dotenv
from supabase import create_client

load_dotenv()

supabase = create_client(
    os.environ["SUPABASE_URL"],
    os.environ["SUPABASE_KEY"]
)

def get_banks():
    response = supabase.table("banks").select("*").execute()
    return response.data

def get_accounts():
    response = supabase.table("accounts").select("*").execute()
    return response.data

def get_accounttypes():
    response = supabase.table("accounttypes").select("*").execute()
    return response.data

def get_transactions():
    response = supabase.table("transactions").select("*").execute()
    return response.data

def get_active_account_balances():
    response = supabase.table("active_account_balances").select("*").execute()
    return response.data

def get_active_account_transactions():
    response = supabase.table("active_account_transactions").select("*").execute()
    return response.data
