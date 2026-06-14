from fastapi import FastAPI
from db import get_banks, get_accounts, get_accounttypes, get_transactions, get_active_account_balances, get_active_account_transactions

app = FastAPI()

@app.get("/")
def root():
    return {"status": "API läuft"}

@app.get("/banks")
def banks():
    return get_banks()

@app.get("/accounts")
def accounts():
    return get_accounts()

@app.get("/accounttypes")
def accounttypes():
    return get_accounttypes()

@app.get("/transactions")
def transactions():
    return get_transactions()

@app.get("/active-account-balances")
def active_account_balances():
    return get_active_account_balances()

@app.get("/active-account-transactions")
def active_account_transactions():
    return get_active_account_transactions()