import {Inject, Service} from "typedi";
import IMedicalRecordRepo from "../services/IRepos/IMedicalRecordRepo";
import {Document, Model} from "mongoose";
import {IMedicalRecordPersistence} from "../dataschema/IMedicalRecordPersistence";
import {MedicalRecord} from "../domain/MedicalRecord/MedicalRecord";
import {MedicalRecordMapper} from "../mappers/MedicalRecordMapper";
import {ObjectId} from "mongodb";


@Service()
export default class MedicalRecordRepo implements IMedicalRecordRepo {
  constructor(@Inject('medicalRecordSchema') private medicalRecordSchema: Model<IMedicalRecordPersistence & Document>,) {
  }

  exists(t: MedicalRecord): Promise<boolean> {
    return Promise.resolve(false);
  }

  public async save(medicalRecord: MedicalRecord, medicalRecordId?: string): Promise<MedicalRecord> {

    const rawMedicalRecord: any = MedicalRecordMapper.toPersistence(medicalRecord, medicalRecordId);

    const medicalRecordCreated = await this.medicalRecordSchema.create(rawMedicalRecord);

    return MedicalRecordMapper.toDomain(medicalRecordCreated);
  }


  private async getLastId(): Promise<number> {
    let number = 1;

    const medicalRecord = await this.medicalRecordSchema.find();

    if (medicalRecord.length > 0) {
      number = medicalRecord.length + 1;
    }

    return number;
  }

  public async getAll(): Promise<MedicalRecord[]> {
    const medicalRecords = await this.medicalRecordSchema
      .find()
      .populate('medicalRecordConditions')
      .populate('medicalRecordAllergies')
      .populate('freeText')
      .exec();

    return medicalRecords.map(MedicalRecordMapper.toDomain);
  }

  public async getMedicalRecordById(medicalRecordId: string): Promise<MedicalRecord> {
    const medicalRecord = await this.medicalRecordSchema
      .findOne({domainId: medicalRecordId})
      .exec();

    if (!medicalRecord) {
      throw new Error(`Medical record with ID ${medicalRecordId} not found`);
    }

    return MedicalRecordMapper.toDomain(medicalRecord);
  }


}
