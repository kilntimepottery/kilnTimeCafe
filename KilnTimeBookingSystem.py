
from sqlalchemy import create_engine, Column, Integer, String, DateTime, ForeignKey
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship, sessionmaker

Base = declarative_base()

class Customer(Base): 
    __tablename__ = 'customers'
    id = Column(Integer, primary_key=True)
    name = Column(String, nullable=False)
    email=Column(String,  nullable=False, unique=True)

class Booking(Base):
    __tablename__ = 'bookings'
    id = Column(Integer, primary_key=True)
    customer_id = Column(Integer, ForeignKey('customers.id'), nullable = False)
    service = Column(String, nullable=False)
    booking_date = Column(DateTime, nullable=False)
    customer = relationship('Customer')

engine = create_engine('sqlite:///booking_system.db')
Base.metadata.create_all(engine)