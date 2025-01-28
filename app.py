from flask import Flask, render_template, request, jsonify, redirect, url_for, session as login_session
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from database_setup import Base, Customer, Booking, Table, Admin, Service
from datetime import datetime



app = Flask(__name__)
app.secret_key = 'super_secret_key'

engine = create_engine('sqlite:///booking_system.db')
Base.metadata.bind = engine
DBSession = sessionmaker(bind = engine)
session = DBSession()

@app.route('/')
def booking_details():
    return render_template('booking_details.html')

@app.route('/select_time',  methods = ['POST'])
def select_time():
    selected_date = request.form.get('date')
    date_obj = datetime.strptime(selected_date, '%Y-%m-%d')
    day_of_week = date_obj.weekday()

    if day_of_week < 1 or day_of_week > 6: 
        return "Bookings are only available between Tuesday and Sunday", 400
    
    available_times = [f"{hour:02}:00" for hour in range (10, 18)]
    return render_template('select_time.html', date = selected_date, times = available_times)

@app.route('/select_service', methods = ['POST'])
def select_service(): 
    date = request.form.get('date')
    time = request.form.get('time')
    services = session.query(Service).all()
    return render_template('select_service.html', date = date, time= time, service = services)


@app.route('/confirm_booking', methods=['POST'])
def confirm_booking():
    name = request.form.get('name')
    email = request.form.get('email')
    guests = int(request.form.get('guests'))
    date = request.form.get('date')
    time = request.form.get('time')
    service_id = request.form.get('service')

    booking_datetime = datetime.strptime(f"{date} {time}", '%Y-%m-%d %H:%M')

    if guests <= 3: 
        table = session.query(Table).filter_by(size = 3, available = True).first()
    else: 
        table = session.query(Table).filter_by(size = 6, available=True).first()
    if table is None: 
        return jsonify({'message': 'No available tables for the selected time'})
    
    table.available = False
    session.commit()

    new_customer = Customer(name = name, email = email)
    session.add(new_customer)
    session.commit()

    new_booking = Booking(customer_id = new_customer.id, service_id = service_id, booking_date = booking_datetime, table_id = table.id)
    session.add(new_booking)
    session.commit()

    return jsonify({'message': 'Booking made successfully!'})

@app.route('/login', methods=['GET', 'POST'])
def login(): 
    if request.method == 'POST': 
        username = request.form.get('username')
        password = request.form.get('password')
        print(f"Attempting login with username: {username}, password: {password}")
        admin = session.query(Admin).filter_by(username=username, password = password).first()
        if admin: 
            login_session['admin_id'] = admin.id
            print("Login successful")
            return redirect(url_for('admin_view'))
        else: 
            print("Invalid credentials")
            return "Invalid Credentials"
    return render_template('login.html')

@app.route('/admin')
def admin_view(): 
    if 'admin_id' not in login_session: 
        return redirect(url_for('login'))
    bookings = session.query(Booking).all()
    return render_template('admin_view.html', bookings = bookings)

if __name__ == '__main__': 
    app.run(debug = True)

