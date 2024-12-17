import {Inject, Service} from "typedi";
import IMedicalRecordRepo from "../services/IRepos/IMedicalRecordRepo";
import {Document, Model} from "mongoose";
import {IMedicalRecordPersistence} from "../dataschema/IMedicalRecordPersistence";
import {MedicalRecord} from "../domain/MedicalRecord/MedicalRecord";
import {MedicalRecordMapper} from "../mappers/MedicalRecordMapper";


@Service()
export default class MedicalRecordRepo implements IMedicalRecordRepo{
    constructor(@Inject('medicalRecordSchema') private medicalRecordSchema: Model<IMedicalRecordPersistence & Document>,) {
    }

    exists(t: MedicalRecord): Promise<boolean> {
        return Promise.resolve(false);
    }

    public async save(medicalRecord: MedicalRecord): Promise<MedicalRecord> {

      const id = await this.getLastId();

      const rawMedicalRecord: any = MedicalRecordMapper.toPersistence(medicalRecord, id);

      const medicalRecordCreated = await this.medicalRecordSchema.create(rawMedicalRecord);

      return MedicalRecordMapper.toDomain(medicalRecordCreated);
    }


  public async getLastId(): Promise<number> {
    let number = 1;

    const medicalRecord = await this.medicalRecordSchema.find();

    if (medicalRecord.length > 0) {
      number = medicalRecord.length + 1;
    }

    return number;
  }

}
