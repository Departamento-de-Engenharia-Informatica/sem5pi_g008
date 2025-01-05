import {IMedicalConditionPersistence} from "../../dataschema/IMedicalConditionPersistence";

import mongoose from 'mongoose';

const MedicalConditionSchema = new mongoose.Schema(
    {
        domainId: { type: Number, unique: true },
        code: { type: String, unique: true, required: true },
        designation: { type: String, unique: true, required: true },
        description: { type: String },
        symptomsList: { type: [String] }
    },
    {
        timestamps: true
    }
);

export default mongoose.model<IMedicalConditionPersistence & mongoose.Document>('MedicalCondition', MedicalConditionSchema);
