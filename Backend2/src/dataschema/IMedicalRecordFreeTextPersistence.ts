import mongoose from "mongoose";

export interface IMedicalRecordFreeTextPersistence {
  domainId: number;
  medicalRecordId: mongoose.Types.ObjectId;
  doctorId: string;
  comment: string;
}
