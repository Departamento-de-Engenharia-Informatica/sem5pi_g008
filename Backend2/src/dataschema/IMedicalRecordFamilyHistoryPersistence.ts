import mongoose from "mongoose";

export interface IMedicalRecordFamilyHistoryPersistence {
  domainId: number;
  medicalRecordId: mongoose.Types.ObjectId;
  familyMember:  string;
  condition: string;
}
 