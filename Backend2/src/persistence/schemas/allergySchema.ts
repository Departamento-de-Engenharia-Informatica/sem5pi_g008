import mongoose from 'mongoose';
import {IAllergyPersistence} from "../../dataschema/IAllergyPersistence";

const AllergySchema = new mongoose.Schema(
  {
      domainId: { type: Number, unique: true },
      code: { type: String, required: true, unique: true },
      designation: { type: String, required: true, unique: true },
      description: { type: String, required: true },
      effects: { type: [String] },
      isDeleted: { type: Boolean, default: false }
  },
  {
      timestamps: true
  }
);

export default mongoose.model<IAllergyPersistence & mongoose.Document>('Allergy', AllergySchema);
