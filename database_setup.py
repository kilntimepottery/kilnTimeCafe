
from sqlalchemy import create_engine, Column, Integer, String, DateTime, ForeignKey, Boolean, Float
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship, sessionmaker

Base = declarative_base()

class Table(Base): 
    __tablename__ = 'tables'
    id = Column(Integer, primary_key = True)
    size = Column(Integer, nullable = False)
    available = Column(Boolean, default = True)


class Customer(Base): 
    __tablename__ = 'customers'
    id = Column(Integer, primary_key=True)
    name = Column(String, nullable=False)
    email=Column(String,  nullable=False, unique=True)

class Booking(Base):
    __tablename__ = 'bookings'
    id = Column(Integer, primary_key=True)
    customer_id = Column(Integer, ForeignKey('customers.id'), nullable = False)
    table_id = Column(Integer, ForeignKey('tables.id'), nullable = False)
    service = Column(String, nullable=False)
    booking_date = Column(DateTime, nullable=False)
    customer = relationship('Customer')
    table= relationship('Table')

class Service(Base): 
    __tablename__= 'services'
    id = Column(Integer, primary_key = True)
    name = Column(String, nullable=False)
    description = Column(String, nullable = False)
    duration = Column(Integer, nullable = False)
    price = Column(Float, nullable = False)

class Admin(Base):
    __tablename__ = 'admins'
    id = Column(Integer, primary_key = True)
    username = Column(String, nullable= False, unique = True)
    password = Column(String, nullable = False)

engine = create_engine('sqlite:///booking_system.db')
Base.metadata.drop_all(engine)
Base.metadata.create_all(engine)
DBSession = sessionmaker(bind = engine)
session = DBSession()

if not session.query(Table).count():
    small_tables = [Table(size = 3) for _ in range(3)]
    large_tables = [Table(size = 6) for _ in range(3)]
    session.add_all(small_tables + large_tables)
    session.commit()

if not session.query(Admin).count():
    admin = Admin(username = 'admin', password = 'adminpass')
    session.add(admin)
    session.commit()

if not session.query(Service).count(): 
    services = [
        Service(name = 'Pottery Painting', description = 'Relax in our welcoming studio space and paint your own pottery masterpiece. Pieces are priced individually depending on the selected item. Pottery will be fired in our kiln and available for pickup within 7 days of painting.', duration=120, price=15)

    ]
    session.add_all(services)
    session.commit()