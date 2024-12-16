import mongoose from 'mongoose';

export interface IMedicalRecordConditionPersistence {
  domainId: number;
  condition: mongoose.Types.ObjectId;
  medicalRecordId: mongoose.Types.ObjectId;
  doctorId: string;
  comment: string;
}
